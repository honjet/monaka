{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Monaka.Markov
import           Monaka.Poetry
import           Text.MeCab
import           Web.Twitter

main :: IO ()
main = do
    tweets <- collectTweets 500
    let text = T.unlines tweets
    markov <- markovNodes text
    let poem = findPoemFromNodes [5,7,5,7,7] markov
    TIO.putStrLn text
    TIO.putStrLn $ fromMaybe "短歌が見つかりませんでした。" poem
