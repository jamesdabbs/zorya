module Util
  ( pprint
  , sleep
  ) where

import Types

import Control.Concurrent         (threadDelay)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Attoparsec.Lazy       (Result(..), parse)
import Data.Aeson                 (json')
import Data.Aeson.Encode.Pretty   (encodePretty)

pprint :: ByteString -> ByteString
pprint s = case parse json' s of
  Done _ v -> encodePretty v
  _        -> s

sleep :: Frequency -> IO ()
sleep (Hours   n) = sleep $ Minutes (60 * n)
sleep (Minutes n) = sleep $ Seconds (60 * n)
sleep (Seconds n) = threadDelay $ n * 1000000
