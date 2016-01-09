module RSS
  ( checkFeeds
  ) where

import           Control.Lens           ((^.))
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text.Lazy         as TL
import           Data.Time.Calendar     (toGregorian)
import           Data.Time.Clock        (getCurrentTime, utctDay)
import           Database.Persist
import qualified Network.Wreq           as Wreq
import           Text.Feed.Import       (parseFeedSource)
import           Text.Feed.Query
import qualified Text.Feed.Types        as RF
import           Text.Regex.Posix       ((=~))

import Types
import DB    (runDB)
import Model
import Slack (slack, slackA)

checkFeeds :: Z ()
checkFeeds = getFeeds >>= mapM_ runFeed

getFeeds :: Z [Entity Feed]
getFeeds = runDB $ selectList [] []

runFeed :: Entity Feed -> Z ()
runFeed (Entity _id feed) = do
  slack "#_robots" $ "Checking *" <> (toStrict $ feedTitle feed) <> "* for new entries"
  es <- liftIO $ fetchFeed feed
  mapM_ (process _id) es

fetchFeed :: Feed -> IO [RF.Item]
fetchFeed feed = do
  r <- Wreq.get . TL.unpack $ feedUrl feed
  let rss = r ^. Wreq.responseBody
      mf  = parseFeedSource rss
  case mf of
    Nothing -> return []
    Just f  -> return $ feedItems f

-- TODO: clean this up
process :: FeedId -> RF.Item -> Z ()
process _id item = case getItemId item of
  Nothing        -> return ()
  Just (_, guid) -> do
    note <- liftIO $ noteworthy item
    when note $ do
      record <- liftIO $ feedItemFromRemote _id guid item
      r <- runDB $ insertBy record
      case r of
        Left  _ -> return () -- Record already exists
        Right _ -> do
          let msg = TL.toStrict $ feedItemTitle record
              url = TL.toStrict $ feedItemDownloadUrl record
          slackA "#rss" msg [("Download", url)]

noteworthy :: RF.Item -> IO Bool
noteworthy = recent

recent :: RF.Item -> IO Bool
recent item = do
  cy <- getCurrentYear
  case itemYear item of
    Nothing -> return True
    Just iy -> return $ iy >= cy - 1

getCurrentYear :: IO Integer
getCurrentYear = do
  t <- getCurrentTime
  let (y,_,_) = toGregorian . utctDay $ t -- Ignoring time zone issues at year boundaries
  return y

itemYear :: RF.Item -> Maybe Integer
itemYear item =
  let title = fromMaybe "" $ getItemTitle item
      patt  = "\\[(20[0-9][0-9])\\]" :: String
  in
    case title =~ patt of
      (_:match:_) : _ -> Just $ read match
      _             -> Nothing

feedItemFromRemote :: FeedId -> String -> RF.Item -> IO FeedItem
feedItemFromRemote feedId itemId ri = do
  let f t = TL.pack $ fromMaybe "" t
      title = f $ getItemTitle ri
      desc  = f $ getItemDescription ri
      url   = f $ getItemLink ri
  now <- getCurrentTime
  return $ FeedItem feedId (TL.pack itemId) title desc url now
