module Util
  ( pprint
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Attoparsec.Lazy       (Result(..), parse)
import Data.Aeson                 (json')
import Data.Aeson.Encode.Pretty   (encodePretty)

pprint :: ByteString -> ByteString
pprint s = encodePretty $
  case parse json' s of
    Done _ v -> v
    _        -> error "Invalid JSON"
