module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader   (runReaderT)
import Data.String            (IsString(..))
import System.Environment     (lookupEnv)

import           Types
import           Bot      (slack, runRtmBot)
import           DB       (poolFromConnString, runMigrations)
import           Download (downloadAttachedItem)
import qualified Rabbit
import           RSS      (checkFeeds)
import           Schedule (runScheduler)

env :: Read v => String -> v -> IO v
env key fallback = maybe fallback read <$> lookupEnv key

requireEnv :: IsString v => String -> IO v
requireEnv key = maybe failure fromString <$> lookupEnv key
  where
    failure = error $ "Required key missing: " ++ key

getBotConf :: IO BotConf
getBotConf = do
  let botName = "Zorya"
  apiToken      <- requireEnv "SLACK_API_TOKEN"
  dbPool        <- requireEnv "DATABASE_URL" >>= poolFromConnString
  rabbitChannel <- getRabbitConf >>= Rabbit.runRabbit
  return BotConf{..}

getRabbitConf :: IO RabbitConf
getRabbitConf = do
  rabbitHost  <- requireEnv "RABBIT_HOST"
  rabbitVHost <- requireEnv "RABBIT_VHOST"
  rabbitUser  <- requireEnv "RABBIT_USERNAME"
  rabbitPass  <- requireEnv "RABBIT_PASSWORD"
  return RabbitConf{..}

main :: IO ()
main = do
  conf <- getBotConf

  runMigrations conf

  let runIO a = runReaderT (runSlack a) conf

  runIO $ slack "#_robots" "/me is waking up"

  Rabbit.bindHandlers runIO (rabbitChannel conf) amqpHandlers
  runScheduler        runIO scheduledEvents
  runRtmBot           runIO (apiToken conf) slackDirectives

  runIO $ slack "#_robots" "/me is going to sleep"

slackDirectives :: Event -> Slack ()
slackDirectives event = case event of
  StarAdded{..} -> downloadAttachedItem item
  -- EventMessage Message{..} -> unless (msgUser == "Zorya") $ slack robots msgText
  _ -> return ()

scheduledEvents :: [ (Frequency, Slack ()) ]
scheduledEvents =
  [ (Hours  12, checkFeeds)
  -- , (Minutes 1, liftIO $ putStrLn "Still alive")
  ]

amqpHandlers :: [ (Queue, RabbitHandler) ]
amqpHandlers =
  [ (Rabbit.logQ, Rabbit.echoToSlack) ]
