{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Applicative        ((<|>))
import Control.Monad.Reader       (MonadIO, MonadReader, ReaderT)
import Data.Aeson
import Data.Aeson.Types           (Parser)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text                  (Text)

import qualified Network.AMQP as AMQP
import qualified Database.Persist.Postgresql as P

data BotConf = BotConf
             { botName       :: !Text
             , apiToken      :: !Text
             , rtorrentUrl   :: !Text
             , rabbitChannel :: AMQP.Channel
             , dbPool        :: P.ConnectionPool
             }

newtype Z a = Z
  { runZ :: ReaderT BotConf IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader BotConf)

type TimeStamp = Text

data AttachmentField = AttachmentField
                     { afValue :: Text
                     , afShort :: Bool
                     , afTitle :: Text
                     } deriving Show

instance FromJSON AttachmentField where
  parseJSON = withObject "attachment field" $ \v -> do
    afValue <- v .: "value"
    afShort <- v .: "short"
    afTitle <- v .: "title"
    return AttachmentField{..}
instance ToJSON AttachmentField where
  toJSON AttachmentField{..} = object
    [ "value" .= afValue
    , "short" .= afShort
    , "title" .= afTitle
    ]

data Attachment = Attachment
                { atId       :: Int
                , atFallback :: Text
                , atFields   :: [AttachmentField]
                } deriving Show
instance FromJSON Attachment where
  parseJSON = withObject "attachment" $ \v -> do
    atId       <- v .: "id"
    atFallback <- v .: "fallback"
    atFields   <- v .:? "fields" .!= []
    return Attachment{..}
instance ToJSON Attachment where
  toJSON Attachment{..} = object
    [ "fields" .= atFields
    ]


-- Question: what's the right way to unify this with the message type?
-- FIXME: this presupposes a `message` item
data Item = Item
          { itChannel     :: Text
          , itUser        :: Text
          , itStarred     :: Bool
          , itAttachments :: [Attachment]
          , itText        :: Text
          , itPermalink   :: Text
          } deriving Show
instance FromJSON Item where
  parseJSON = withObject "item" $ \v -> do
    itChannel     <- v .: "channel"
    message       <- v .: "message"

    itUser        <- message .: "user"
    itStarred     <- message .:? "is_starred" .!= False
    itAttachments <- message .: "attachments"
    itText        <- message .: "text"
    itPermalink   <- message .: "permalink"
    return $ Item{..}

data Message = Message
             { msgChannel   :: Text
             , msgUser      :: Text
             , msgText      :: Text
             , msgTs        :: TimeStamp
             } deriving Show
instance FromJSON Message where
  parseJSON = withObject "message" $ \v -> do
    msgChannel   <- v .: "channel"
    msgUser      <- v .: "user" <|> v .: "username" -- N.B. Bots have "username"s, users have "user"s (ids)
    msgText      <- v .: "text"
    msgTs        <- v .: "ts"
    return Message{..}


data Event = EventMessage Message
           | StarAdded { userId :: Text, ts :: TimeStamp, item :: Item }
           | MessageResponse
           | MessageError
           | UnknownEvent
           deriving Show
instance FromJSON Event where
  parseJSON = withObject "event" $ \v -> do
    typ <- v .:? "type"
    case typ of
      Just t  -> parseEvent t $ Object v
      Nothing -> do
        ok <- v .: "ok"
        if ok
          then return MessageResponse -- <$> v .: "reply_to" <*> v .: "ts" <*> v .: "text"
          else return MessageError -- <$> v .: "reply_to" <*> v .: "error"

parseEvent :: Text -> Value -> Parser Event
parseEvent t = withObject "event" $ \v ->
  case t of
    "message" -> do
      m <- parseJSON $ Object v
      return $ EventMessage m
    "star_added" -> do
      userId <- v .: "user"
      ts     <- v .: "event_ts"
      item   <- v .: "item"
      return StarAdded{..}
    _ -> return UnknownEvent

type Queue = Text

data RabbitConf = RabbitConf
                { rabbitHost  :: !String
                , rabbitVHost :: !Text
                , rabbitUser  :: !Text
                , rabbitPass  :: !Text
                } deriving Show

data Frequency = Hours Int | Minutes Int | Seconds Int

type RabbitHandler = Text -> AMQP.Message -> ByteString -> Z ()
