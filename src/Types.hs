{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (MonadReader, runReaderT, ReaderT)
import           Data.Text                 (Text)
import qualified Data.Text.Lazy as L
import           Web.Scotty.Trans          (ActionT)

data Environment = Development | Test | Production deriving (Eq, Read, Show)

type Port = Int

data Config = Config
  { envrionment :: Environment
  , port        :: Port
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = L.Text

type Action = ActionT Error ConfigM

type ApiToken = String

type Channel = Text

type Message = String
