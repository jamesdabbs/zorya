module Bot
  ( slack
  , slackA
  , runRtmBot
  , robots
  ) where

import           Control.Lens               ((.~), (&), (^?), (^.))
import           Control.Monad              (forever, unless)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ask)
import           Data.Aeson                 (eitherDecode, encode)
import           Data.Aeson.Lens            (Primitive(..), _Primitive, _String, key)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
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

slack :: Channel -> T.Text -> Slack ()
slack channel text = slackA channel text []

slackA :: Channel -> T.Text -> [(T.Text, T.Text)] -> Slack ()
slackA channel text attached = do
  conf <- ask
  let opts = defaults
           & param "token"       .~ [apiToken conf]
           & param "channel"     .~ [channel]
           & param "text"        .~ [text]
           & param "username"    .~ [botName conf]
           & param "as_user"     .~ ["false" :: T.Text]
           & param "icon_emoji"  .~ [":star2:" :: T.Text]
           & param "attachments" .~ [formatAttachments attached]
  let body = [] :: [FormParam]

  _ <- liftIO $ putStrLn $ T.unpack $ formatAttachments attached

  _ <- liftIO $ postWith opts "http://slack.com/api/chat.postMessage" body
  -- TODO: check response, handle non-200s
  return ()

formatAttachments :: [(T.Text, T.Text)] -> T.Text
formatAttachments pairs =
  decodeUtf8 . BC.toStrict . encode $ map mkAttachment pairs
    where
      mkAttachment (k,v) = Attachment
        { atId       = -1
        , atFallback = ""
        , atFields =
          [ AttachmentField
            { afTitle = k
            , afValue = v
            , afShort = False
            }
          ]
        }

runRtmBot :: (Slack () -> IO ()) -> T.Text -> (Event -> Slack ()) -> IO ()
runRtmBot runIO token directives = do
  (host, path) <- getSlackWebsocket token
  SSL.withOpenSSL $ do
    stream <- getStreamForWebsocket host
    WS.runClientWithStream stream host path WS.defaultConnectionOptions [] wsApp
  where
    wsApp ws = do
      WS.forkPingThread ws 10
      runIO . forever $ handleEvents directives ws


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


robots :: Channel
robots = "#_robots"

handleEvents :: (Event -> Slack ()) -> WS.Connection -> Slack ()
handleEvents directives ws = do
  raw <- liftIO $ WS.receiveData ws
  case eitherDecode raw of
    Left e -> liftIO $ do
      putStrLn $ "Failed to parse event: " ++ e
      BC.putStrLn $ pprint raw
      putStrLn ""
    Right event -> directives event
