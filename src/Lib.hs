{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( bootSlackbot
    , send
    ) where

import Types

import           Control.Lens              ((&), (^.), (^?), (.~))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (MonadReader, runReaderT, ReaderT)
import           Network.Wreq              (postWith, defaults, param, FormParam(..), Response)

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Web.Slack                 as SB
import qualified Web.Slack.Message         as SB

bootSlackbot :: ApiToken -> IO ()
bootSlackbot apiToken = do
  let rtmConf = SB.SlackConfig { SB._slackApiToken = apiToken }
  SB.runBot rtmConf (slackbot apiToken) ()

slackbot :: ApiToken -> SB.Event -> SB.Slack () ()
slackbot token (SB.Message cid _ msg _ _ _) = do
  let channel = cid ^. SB.getId
  liftIO $ send token channel msg
  return ()
slackbot token (SB.StarAdded uid item _) = do
  liftIO $ send token "#_robots" "Star added!"
slackbot _ _ = return ()

send :: String -> Channel -> T.Text -> IO ()
send token channel text = do
  let opts = defaults
           & param "token"      .~ [T.pack token]
           & param "channel"    .~ [channel]
           & param "text"       .~ [text]
           & param "username"   .~ [("Zorya" :: T.Text)]
           & param "as_user"    .~ [("false" :: T.Text)]
           & param "icon_emoji" .~ [(":star2:" :: T.Text)]
  let body = [] :: [FormParam]

  response <- postWith opts "http://slack.com/api/chat.postMessage" body
  -- TODO: check response, handle non-200s
  putStrLn $ show response
  return ()
