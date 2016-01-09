module Rabbit
  ( bindHandlers
  , echoToSlack
  , doRpc
  , logQ
  , rpcQ
  , runRabbit
  , rpcDownload
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (asks)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Data.Text.Lazy             (fromStrict)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import           Network.AMQP               hiding (Channel, Message)
import qualified Network.AMQP               as AMQP
import           Network.URI                (unEscapeString)

import           Types
import qualified Slack
import           Download (doDownload)
import           Util     (pprint)

rpcQ, logQ, downloadQ :: Queue
rpcQ      = "rpc"
logQ      = "log"
downloadQ = "download"

runRabbit :: RabbitConf -> IO AMQP.Channel
runRabbit RabbitConf{..} = do
  conn <- openConnection rabbitHost rabbitVHost rabbitUser rabbitPass
  chan <- openChannel conn

  declareExchange chan newExchange
    { exchangeName = "notifications"
    , exchangeType = "topic"
    , exchangeDurable = True
    }

  _ <- declareQueue chan newQueue { queueName = logQ, queueDurable = False }
  bindQueue chan logQ "notifications" "#"

  _ <- declareQueue chan newQueue { queueName = downloadQ, queueDurable = True }
  bindQueue chan downloadQ "notifications" "download"

  _ <- declareQueue chan newQueue { queueName = "rpc", queueDurable = False }
  bindQueue chan "rpc" "notifications" "rpc.#"
  return chan

replyTo :: AMQP.Channel -> AMQP.Message -> BC.ByteString -> IO ()
replyTo chan msg body = case msgReplyTo msg of
  Nothing -> return ()
  Just rt -> publishMsg chan "" rt $
    newMsg { msgBody = body, msgCorrelationID = msgCorrelationID msg }

bindHandlers :: (Z () -> IO ()) -> AMQP.Channel -> [ (Queue, RabbitHandler) ] -> IO ()
bindHandlers runIO chan hs = mapM_ bindOne hs
  where
    bindOne (q,h) = consumeMsgs chan q Ack $ \(msg, env) -> do
      runIO $ h (envRoutingKey env) msg (msgBody msg)
      ackEnv env

doRpc :: RabbitHandler
doRpc k m payload = do
  ch <- asks rabbitChannel
  let reply = liftIO . replyTo ch m
  case k of
    "rpc.ping" -> do
      name <- asks botName
      reply $ "pong (" <> (BC.pack $ T.unpack name) <> ")"
    "rpc.download" -> do
      doDownload payload
      reply $ "Downloading " <> payload
    other -> Slack.debug $ "Unknown RPC key - " <> other

echoToSlack :: RabbitHandler
echoToSlack k _ body = do
  let body' = T.pack . BC.unpack . pprint $ body
      msg   = "*" <> k <> "*\n```" <> body' <> "```\n"
  Slack.debug msg

rpcDownload :: T.Text -> Z ()
rpcDownload link = do
  chan <- asks rabbitChannel
  liftIO $ publishMsg chan "notifications" "rpc.download"
      -- TODO: ugh
      newMsg { msgBody         = encodeUtf8 . fromStrict . T.pack . unEscapeString . T.unpack $ link
             , msgDeliveryMode = Just Persistent
             }
