{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent     (forkFinally)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader   (runReaderT)
import Data.Aeson             hiding (json)
import Data.Maybe             (fromMaybe)
import Data.Text              (Text)
import System.Environment     (lookupEnv)
import Web.Scotty.Trans       hiding (param)

import Lib (bootSlackbot, send)
import Types

getConfig :: IO Config
getConfig = Config
    <$> env "SCOTTY_ENV" Development
    <*> env "PORT"       9899

env :: Read v => String -> v -> IO v
env key fallback = maybe fallback read <$> lookupEnv key

requireEnv :: String -> IO String
requireEnv key = fromMaybe failure <$> lookupEnv key
  where
    failure = error $ "Required key missing: " ++ key

workerExitNotify :: Either SomeException a -> IO ()
workerExitNotify _ = do
  putStrLn $ "Worker thread exited"

respondWith :: [(Text, Value)] -> Action ()
respondWith = json . object

-- p :: String -> SB.Slack () ()
-- p = liftIO . putStrLn

robots :: Channel
robots = "#_robots" -- "#G087UQUDA"

application :: ScottyT Error ConfigM ()
application = do
  get "/" $ do
    name <- liftIO $ requireEnv "URL"
    respondWith [ "status" .= ("ok"::Text), "name" .= name ]

boot :: IO ()
boot = do
  conf <- getConfig
  let run m = runReaderT (runConfigM m) conf
  scottyT (port conf) run application

main :: IO ()
main = do
  _ <- forkFinally boot workerExitNotify

  apiToken <- requireEnv "SLACK_API_TOKEN"
  send apiToken robots "Waking up!"
  bootSlackbot apiToken

  return ()
