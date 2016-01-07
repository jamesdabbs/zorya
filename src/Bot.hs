{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Control.Lens               ((.~), (&), (^?), (^.))
import           Control.Monad              (forever, unless)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (runReaderT, ask)
import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Lens            (Primitive(..), _Primitive, _String, key)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Foldable              (forM_)
import           Data.Monoid                ((<>))
import qualified Data.List                  as L
import qualified Data.Text                  as T
import           Network.AMQP               hiding (Channel, Message)
import qualified Network.AMQP               as AMQP
import qualified Network.Socket             as S
import qualified Network.URI                as URI
import qualified Network.WebSockets         as WS
import qualified Network.WebSockets.Stream  as WS
import           Network.Wreq               (FormParam, get, postWith, defaults, param, responseBody)
import qualified OpenSSL                    as SSL
import qualified OpenSSL.Session            as SSL
import qualified System.IO.Streams.Internal as StreamsIO
import           System.IO.Streams.SSL      (sslToStreams)


import Types
import Util

-- Actual business logic

directives :: Directives
directives event = do
  case event of
    EventMessage Message{..} -> do
      unless (msgUser == "Zorya") $ do
        send robots msgText

    StarAdded{..} -> forM_ (starredDownloadLink item) downloadLink

    _ -> return ()
  return ()

echoLogToSlack, notifyOfNewRssItems :: T.Text -> BC.ByteString -> Slack ()
echoLogToSlack k body = do
  let body' = T.pack . BC.unpack . pprint $ body
      msg   = "*" <> k <> "*\n```" <> body' <> "```\n"
  send robots msg

notifyOfNewRssItems _ _ = do
  send rss "Got new item"

downloadLink :: T.Text -> Slack ()
downloadLink url = do
  send robots $ "Should download link: " <> url


-- Infrastructure and helpers

starredDownloadLink :: Item -> Maybe T.Text
starredDownloadLink item = afValue <$> L.find match fields
  where
    fields  = concat . map atFields $ itAttachments item
    match a = afTitle a == "Download"

parseWebsocket :: T.Text -> (S.HostName, String)
parseWebsocket url =
  let
    parsed = do
      uri  <- URI.parseURI $ T.unpack url
      name <- URI.uriRegName <$> URI.uriAuthority uri
      return (name, URI.uriPath uri)
  in case parsed of
    Just (name, path) -> (name, path)
    _                 -> error $ "Couldn't parse Slack websocket URL from " ++ T.unpack url

getSlackWebsocket :: T.Text -> IO (S.HostName, String)
getSlackWebsocket token = do
  r <- get $ "https://slack.com/api/rtm.start?token=" ++ (T.unpack token)
  let Just (BoolPrim ok) = r ^? responseBody . key "ok"  . _Primitive
  unless ok $ do
    putStrLn "Unable to connect"
    ioError . userError . T.unpack $ r ^. responseBody . key "error" . _String
  let Just url = r ^? responseBody . key "url" . _String
  return $ parseWebsocket url

getStreamForWebsocket :: S.HostName -> IO WS.Stream
getStreamForWebsocket host = do
  ctx <- SSL.context
  is  <- S.getAddrInfo Nothing (Just host) (Just "443")
  let a = S.addrAddress $ head is
      f = S.addrFamily $ head is
  s <- S.socket f S.Stream S.defaultProtocol
  S.connect s a
  ssl <- SSL.connection ctx s
  SSL.connect ssl
  (i,o) <- sslToStreams ssl
  WS.makeStream  (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o )

runBot :: BotConf -> RabbitConf -> IO ()
runBot bc rc = do
  (host, path) <- getSlackWebsocket $ apiToken bc
  SSL.withOpenSSL $ do
    stream <- getStreamForWebsocket host
    WS.runClientWithStream stream host path WS.defaultConnectionOptions [] (mkBot bc rc)

getConf :: Slack BotConf
getConf = ask

send :: Channel -> T.Text -> Slack ()
send channel text = do
  conf <- getConf
  let opts = defaults
           & param "token"      .~ [apiToken conf]
           & param "channel"    .~ [channel]
           & param "text"       .~ [text]
           & param "username"   .~ [botName conf]
           & param "as_user"    .~ [("false" :: T.Text)]
           & param "icon_emoji" .~ [(":star2:" :: T.Text)]
  let body = [] :: [FormParam]

  _ <- liftIO $ postWith opts "http://slack.com/api/chat.postMessage" body
  -- TODO: check response, handle non-200s
  return ()


rssQ, logQ :: Queue
rssQ = "rss"
logQ = "log"

robots, rss :: Channel
robots = "#_robots"
rss    = "rss"

onMessage :: AMQP.Channel -> T.Text -> (T.Text -> BC.ByteString -> IO ()) -> IO ()
onMessage chan q handler = do
  _ <- consumeMsgs chan q Ack $ \(msg, env) -> do
    handler (envRoutingKey env) (msgBody msg)
    ackEnv env
  return ()

startRabbit :: RabbitConf -> IO AMQP.Channel
startRabbit RabbitConf{..} = do
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

mkBot :: BotConf -> RabbitConf -> WS.ClientApp ()
mkBot bc rc conn = do
  WS.forkPingThread conn 10

  chan <- startRabbit rc
  onMessage chan logQ (runH echoLogToSlack)
  onMessage chan rssQ (runH notifyOfNewRssItems)

  runIO $ do
    send robots "/me is waking up"
    liftIO $ putStrLn "Ready to go!"
    _ <- forever loop
    send robots "/me is going to sleep"

  where
    runIO :: Slack () -> IO ()
    runIO a = runReaderT a bc

    -- FIXME: clean this up
    runH h k v = runIO $ h k v

    loop = do
      raw <- liftIO $ WS.receiveData conn
      case eitherDecode raw of
        Left e -> liftIO $ do
          putStrLn $ "Failed to parse event: " ++ e
          BC.putStrLn $ pprint raw
          putStrLn ""
        Right event -> directives event

