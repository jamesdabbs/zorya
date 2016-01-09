module Download
  ( doDownload
  , starredDownloadLink
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (asks)
import qualified Data.List                  as L
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  (Text, pack, unpack)
import           Network.XmlRpc.Client      (remote)

import Types
import Slack (slackA)

starredDownloadLink :: Item -> Maybe Text
starredDownloadLink item = afValue <$> L.find match fields
  where
    fields  = concat . map atFields $ itAttachments item
    match a = afTitle a == "Download"

doDownload :: BC.ByteString -> Z ()
doDownload link = do
  rurl <- asks rtorrentUrl
  resp <- liftIO $ remote (unpack rurl) "load_start" (BC.unpack link) :: Z Int
  slackA "#_robots" "New download"
    [ ( "Link",     pack $ BC.unpack link )
    , ( "Response", pack $ show resp )
    ]
