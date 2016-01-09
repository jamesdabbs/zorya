{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Data.Text.Lazy (Text)
import Data.Time.Clock (UTCTime)

import Database.Persist.TH (mkMigrate, mkPersist, persistFileWith, share, sqlSettings)
import Database.Persist.Quasi (lowerCaseSettings)

share [mkMigrate "migrateAll", mkPersist sqlSettings] $(persistFileWith lowerCaseSettings "db/schema")
