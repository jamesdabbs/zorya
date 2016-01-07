{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String        (IsString(..))
import System.Environment (lookupEnv)

import Types
import Bot (runBot)

env :: Read v => String -> v -> IO v
env key fallback = maybe fallback read <$> lookupEnv key

requireEnv :: IsString v => String -> IO v
requireEnv key = maybe failure fromString <$> lookupEnv key
  where
    failure = error $ "Required key missing: " ++ key

getBotConf :: IO BotConf
getBotConf = do
  let botName = "Zorya"
  apiToken <- requireEnv "SLACK_API_TOKEN"
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
  b <- getBotConf
  r <- getRabbitConf

  runBot b r

  return ()
