module Download
  ( checkForNewDownloads
  , doDownload
  , starredDownloadLink
  ) where

import           Control.Monad              (forM_, unless)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (asks)
import qualified Data.List                  as L
import           Data.Monoid                ((<>))
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text.Lazy             as LT
import           Database.Persist
import           Network.XmlRpc.Client      (remote)
import           Network.XmlRpc.Internals

import Types
import Model
import DB    (runDB, runDB_)
import Util  (sleep)
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
  -- TODO: fork?
  liftIO $ sleep $ Seconds 5
  checkForNewDownloads
  return ()

-- TODO: this is almost certainly the wrong level of abstraction. See coupling with `remote` call below
data RemoteDL = RemoteDL { dlHash :: LT.Text, dlName :: LT.Text, dlComplete :: Bool } deriving Show
instance XmlRpcType RemoteDL where
  toValue _ = error "Not implemented: RemoteDL toValue"

  fromValue v = do
    fs <- fromValue v
    hash     <- fromValue $ fs !! 0
    name     <- fromValue $ fs !! 1
    complete <- fromValue $ fs !! 2
    let complete' = (complete :: Int) == 1
    return RemoteDL { dlHash = LT.pack hash, dlName = LT.pack name, dlComplete = complete' }
  getType _ = TStruct

checkForNewDownloads :: Z ()
checkForNewDownloads = do
  rurl <- asks rtorrentUrl
  resp <- liftIO $ remote (unpack rurl) ("d.multicall"::String) ("main"::String) ("d.hash="::String) ("d.name="::String) ("d.complete="::String)
  forM_ (resp :: [RemoteDL]) recordAndNotify

recordAndNotify :: RemoteDL -> Z ()
recordAndNotify RemoteDL{..} = do
  mold <- runDB . getBy . UniqueDownloadHash $ dlHash
  let new    = Download { downloadHash = dlHash, downloadName = dlName, downloadComplete = dlComplete }
      notice = if dlComplete then "Download complete: " else "Download started: "
      msg    = "*" <> notice <> "*\n```" <> LT.toStrict dlName <> "```"
      notify = slackA "#downloads" msg []
  case mold of
    Nothing -> do
      notify
      runDB_ $ insert new
    Just (Entity _id old) -> unless (dlComplete == downloadComplete old) $ do
      notify
      runDB_ $ replace _id new
