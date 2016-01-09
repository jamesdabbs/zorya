module Main where

import Control.Monad.Reader   (runReaderT)
import Data.Monoid            ((<>))
import Data.Text              (Text, pack, unpack)
import System.Environment     (lookupEnv)

import           Types
import           DB       (poolFromConnString, runMigrations)
import           Handlers (downloadAttachedItem)
import qualified Rabbit   as R
import           RSS      (checkFeeds)
import           Schedule (runScheduler)
import qualified Slack    as S

env :: String -> Text -> IO Text
env key fallback = do
  v <- lookupEnv key
  case v of
    Nothing -> return fallback
    Just v' -> return $ pack v'

requireEnv :: String -> IO Text
requireEnv key = env key failure
  where
    failure = error $ "Required key missing: " ++ key

getBotConf :: IO BotConf
getBotConf = do
  envName       <- env "NAME" "[Dev]"
  let botName = "Zorya " <> envName
  apiToken      <- requireEnv "SLACK_API_TOKEN"
  dbPool        <- requireEnv "DB_URL" >>= poolFromConnString . unpack
  rabbitChannel <- getRabbitConf >>= R.runRabbit
  rtorrentUrl   <- requireEnv "RTORRENT_URL"
  return BotConf{..}

getRabbitConf :: IO RabbitConf
getRabbitConf = do
  rabbitHost  <- unpack <$> requireEnv "RABBIT_HOST"
  rabbitVHost <- requireEnv "RABBIT_VHOST"
  rabbitUser  <- requireEnv "RABBIT_USERNAME"
  rabbitPass  <- requireEnv "RABBIT_PASSWORD"
  return RabbitConf{..}


main :: IO ()
main = do
  conf <- getBotConf

  runMigrations conf

  let runIO a = runReaderT (runZ a) conf

  runIO $ S.debug "is waking up"

  R.bindHandlers runIO (rabbitChannel conf) amqpHandlers
  runScheduler runIO scheduledEvents

  putStrLn "Ready to go!"
  S.runRtmBot runIO (apiToken conf) slackDirectives -- N.B. This one blocks ...

  runIO $ S.debug "/me is going to sleep"

slackDirectives :: Event -> Z ()
slackDirectives event = case event of
  StarAdded{..} -> downloadAttachedItem item
  -- EventMessage Message{..} -> unless (msgUser == "Zorya") $ slack robots msgText
  _ -> return ()

scheduledEvents :: [ (Frequency, Z ()) ]
scheduledEvents =
  [ (Hours 1, checkFeeds)
  -- , (Minutes 1, liftIO $ putStrLn "Still alive")
  ]

amqpHandlers :: [ (Queue, RabbitHandler) ]
amqpHandlers =
  [ (R.logQ, R.echoToSlack)
  , (R.rpcQ, R.doRpc)
  ]
