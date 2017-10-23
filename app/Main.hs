module Main where

import qualified Data.Text as T
import Monaka.Markov (markovChain)
import Monaka.Poetry (findPoem)
import Web.Twitter (collectTweets, tweet)

main :: IO ()
main = do
    tweets <- collectTweets 500
    let text = T.unlines tweets
    markov <- markovChain (T.unpack text)
    poem <- findPoem [5,7,5,7,7] markov
    tweet $ T.pack poem