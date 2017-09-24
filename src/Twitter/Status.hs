module Twitter.Status
  ( Status(..)
  , statusIsRetweet
  ) where

import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as B
import           Data.Csv              (FromField, FromRecord, Record,
                                        parseField, parseRecord, (.!))
import qualified Data.Text             as T
import           GHC.Generics
import           Text.Read             (readMaybe)

data Status = Status { tweetedId          :: Int
                     , replyToId          :: Maybe Int
                     , inReplyToUserId    :: Maybe Int
                     , timestamp          :: UTCTime
                     , tweetedSource      :: String
                     , tweetedText        :: String
                     , retweetedId        :: Maybe Int
                     , retweetedUserId    :: Maybe Int
                     , retweetedTimestamp :: Maybe
                     , expandedUrls       :: String
                     } deriving (Show)


instance FromField UTCTime where
  parseField s = case readMaybe (B.unpack s) of
    Nothing -> fail "failed to parse UTCTime"
    Just t  -> pure t

instance FromRecord Status where
  parseRecord v = Status
                  <$> v .! 0
                  <*> v .! 1
                  <*> v .! 2
                  <*> v .! 3
                  <*> v .! 4
                  <*> v .! 5
                  <*> v .! 6
                  <*> v .! 7
                  <*> v .! 8
                  <*> v .! 9

statusIsRetweet :: Status -> Bool
statusIsRetweet status = case retweetedId status of
  Just _ -> True
  _      -> False
