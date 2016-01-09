module Rabbit
  ( bindHandlers
  , echoToSlack
  , logQ
  , runRabbit
  ) where

import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Network.AMQP               hiding (Channel, Message)
import qualified Network.AMQP               as AMQP

import           Types
import qualified Bot
import           Util  (pprint)

rssQ, logQ :: Queue
rssQ = "rss"
logQ = "log"

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

  _ <- declareQueue chan newQueue { queueName = rssQ, queueDurable = True }
  bindQueue chan rssQ "notifications" "rss.#"

  return chan

bindHandlers :: (Slack () -> IO ()) -> AMQP.Channel -> [ (Queue, RabbitHandler) ] -> IO ()
bindHandlers runIO chan hs = mapM_ bindOne hs
  where
    bindOne (q,h) = consumeMsgs chan q Ack $ \(msg, env) -> do
      runIO $ h (envRoutingKey env) (msgBody msg)
      ackEnv env

echoToSlack :: RabbitHandler
echoToSlack k body = do
  let body' = T.pack . BC.unpack . pprint $ body
      msg   = "*" <> k <> "*\n```" <> body' <> "```\n"
  Bot.slack Bot.robots msg
