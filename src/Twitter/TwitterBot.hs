{-# LANGUAGE OverloadedStrings #-}

module Twitter.TwitterBot
  ( getTwInfoFromEnv
  , textSourceFromRemoteTweetsCSV
  , textSourceFromTweetsCSV
  , postPoemWithTable
  ) where

import           Control.Exception          (catch)
import           Control.Lens               ((^.))
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv                   (HasHeader (..), decode)
import           Data.List                  (intercalate, isInfixOf)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Network.HTTP.Conduit       (newManager, tlsManagerSetting)
import           Network.Wreq               (get, responseBody)
import           System.Environment         (lookupEnv)
import           Web.Authenticate.OAuth     (def, newCredential,
                                             oauthConsumerKey,
                                             oauthConsumerSecret)
import           Web.Twitter.Conduit        (TWInfo,
                                             TwitterError (TwitterErrorResponse),
                                             TwitterErrorMessage (..), call,
                                             setCredential, twitterOAuth,
                                             update)

type URL = String

getTwInfoFromEnv :: IO (Maybe TWInfo)
getTwInfoFromEnv = do
  let Keys = [ "MONAKA_BOT_CONSUMER_KEY"
             , "MONAKA_BOT_CONSUMER_SECRET"
             , "MONAKA_BOT_ACCESS_TOKEN"
             , "MONAKA_BOT_ACCESS_TOKEN_SECRET"
             ]
  [consumerKey, consumerSecret, accessToken, accessTokenSecret] <- map (fmap BS.pack) <$> traverse lookupEnv keys
  let tokens = buildOAuth <$> consumerKey <*> consumerSecret
      credential = newCredential <$> accessToken <*> accessTokenSecret
  return $ setCredential <$> tokens <*> credential <*> pure def
  where
    buildOAuth key secret = twitterOAuth { oauthConsumerKey = key
                                         , oauthConsumerSecret = secret
                                         }

textSourceFromRemoteTweetsCSV :: URL -> IO String
textSourceFromRemoteTweetsCSV = fmap textSourceFromStatusesVector . statusesFromRemoteTweetsCSV

textSourceFromTweetsCSV :: FilePath -> IO String
textSourceFromTweetsCSV = fmap textSourceFromStatusesVector . statusesFromTweetsCSV

decodeTweetsCSV :: BL.ByteString -> V.Vector Status
decodeTweetsCSV = either (error "Failed to parse CSV file") id . decode HasHeader

statusesFromTweetsCSV :: FilePath -> IO (V.Vector Status)
statusesFromTweetsCSV path = decodeTweetsCSV <$> BL.readFile path

statusesFromRemoteTweetsCSV :: URL -> IO (V.Vector Status)
statusesFromRemoteTweetsCSV url = decodeTweetsCSV <$> ((^. responseBody) <$> get url)

textSourceFromStatusesVector :: V.Vector Status -> String
textSourceFromStatusesVector = intercalate "\n"
                               . V.toList
                               . V.map statusText
                               . V.filter (not . is InfixOf "http" . statusText)
                               . V.filter (notElem '@' . statusText)
                               . V.filter (not . statusIsRetweet)
