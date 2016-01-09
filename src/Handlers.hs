module Handlers
  ( downloadAttachedItem
  ) where

import           Data.Foldable              (forM_)

import Types
import Download (starredDownloadLink)
import Rabbit   (rpcDownload)

downloadAttachedItem :: Item -> Z ()
downloadAttachedItem item = forM_ (starredDownloadLink item) rpcDownload
