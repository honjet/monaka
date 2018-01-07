module Main where

import           Data.Maybe
import qualified Data.Text     as T
import           Monaka.Markov (markovNodes)
import           Monaka.Poetry (findPoemFromNodes)
import           Web.Twitter   (collectTweets, tweet)

main :: IO ()
main = do
    tweets <- collectTweets 500
    let text = T.unlines tweets
    markov <- markovNodes text
    let poem = findPoemFromNodes [5,7,5,7,7] markov
    case poem of
        Just p -> tweet p
        _      -> error "短歌生成失敗;;"
