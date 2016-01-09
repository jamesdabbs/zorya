module Download
  ( downloadAttachedItem
  ) where

import           Data.Foldable              (forM_)
import qualified Data.List                  as L
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)

import Types
import Bot   (slack)

downloadAttachedItem :: Item -> Slack ()
downloadAttachedItem item = forM_ (starredDownloadLink item) downloadLink

starredDownloadLink :: Item -> Maybe Text
starredDownloadLink item = afValue <$> L.find match fields
  where
    fields  = concat . map atFields $ itAttachments item
    match a = afTitle a == "Download"

downloadLink :: Text -> Slack ()
downloadLink url = do
  slack "#_robots" $ "Should download link: " <> url
