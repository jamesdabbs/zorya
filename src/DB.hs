{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DB
  ( poolFromConnString
  , runDB
  , runMigrations
  ) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (runStdoutLoggingT, runNoLoggingT)
import           Control.Monad.Reader        (MonadReader, asks)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist.Postgresql (ConnectionPool, SqlPersistT,
                                              createPostgresqlPool, runMigration, runSqlPool)
import           Web.Heroku                  (parseDatabaseUrl)

import Types
import Model (migrateAll)

poolFromConnString :: String -> IO ConnectionPool
poolFromConnString cs = runNoLoggingT $ createPostgresqlPool cs' 10
  where
    cs' = normalize $ parseDatabaseUrl cs
    normalize = encodeUtf8 . T.unwords . map (\(k,v) -> T.concat [k, "='", v, "'"])

runDB :: (MonadReader BotConf m, MonadIO m) =>
         SqlPersistT IO b -> m b
runDB q = do
  p <- asks dbPool
  liftIO $ runSqlPool q p

runMigrations :: BotConf -> IO ()
runMigrations c = runSqlPool (runMigration migrateAll) (dbPool c)
