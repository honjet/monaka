{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Twitter
    ( collectTweets
    , tweet
    ) where

import Data.Aeson
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Conduit
import System.Environment
import Web.Authenticate.OAuth

newtype Tweet = Tweet { text :: String
                      } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

collectTweets :: Int -> IO [Text]
collectTweets twQty = do
    res <- getTweets "monaka57577"
    case res of
        Left  err -> error err
        Right ts  -> mapM (pure . pack . text) $ Prelude.take twQty ts

myOAuth :: Text -> Text -> OAuth
myOAuth key secret = newOAuth { oauthServerName     = "api.twitter.com"
                              , oauthConsumerKey    = encodeUtf8 key
                              , oauthConsumerSecret = encodeUtf8 secret
                              }

myCred :: Text -> Text -> Credential
myCred token tokenSecret = newCredential (encodeUtf8 token) (encodeUtf8 tokenSecret)

getKeysFromEnv :: IO (OAuth, Credential)
getKeysFromEnv = do
    key         <- getEnv "MONAKA_TWITTER_CONSUMER_KEY"
    secret      <- getEnv "MONAKA_TWITTER_CONSUMER_SECRET"
    token       <- getEnv "MONAKA_TWITTER_ACCESS_TOKEN"
    tokenSecret <- getEnv "MONAKA_TWITTER_ACCESS_TOKEN_SECRET"
    return (myOAuth (pack key) (pack secret), myCred (pack token) (pack tokenSecret))

getTweets :: String -> IO (Either String [Tweet])
getTweets name = do
    (oauth, cred) <- getKeysFromEnv
    req           <-
        parseRequest
        $ "https://api.twitter.com/1.1/statuses/home_timeline.json?screen_name="
        ++ name
        ++ "&count=1000"
    m   <- newManager tlsManagerSettings
    res <- do
        signedreq <- signOAuth oauth cred req
        httpLbs signedreq m
    return $ eitherDecode $ responseBody res

tweet :: Text -> IO ()
tweet tw = do
    (oauth, cred) <- getKeysFromEnv
    req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
    m <- newManager tlsManagerSettings
    _ <- do
        signedreq <- signOAuth oauth cred postReq
        httpLbs signedreq m
    return ()

