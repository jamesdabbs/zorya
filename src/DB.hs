{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DB
  ( poolFromConnString
  , runDB
  , runDB_
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
poolFromConnString cs =
  let normalize = encodeUtf8 . T.unwords . map (\(k,v) -> T.concat [k, "='", v, "'"])
      cs'       = normalize $ parseDatabaseUrl cs
      pool      = createPostgresqlPool cs' 10
  in runStdoutLoggingT pool

runDB :: (MonadReader BotConf m, MonadIO m) =>
         SqlPersistT IO b -> m b
runDB q = do
  p <- asks dbPool
  liftIO $ runSqlPool q p

runDB_ :: (MonadReader BotConf m, MonadIO m) =>
         SqlPersistT IO b -> m ()
runDB_ q = runDB q >> return ()

runMigrations :: BotConf -> IO ()
runMigrations c = runSqlPool (runMigration migrateAll) (dbPool c)
