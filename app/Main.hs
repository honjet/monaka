module Main where

import qualified Data.Text     as T
import           Monaka.Markov (markovNodes, markovStr)
import           Monaka.Poetry (findPoem)
import           Web.Twitter   (collectTweets, tweet)

main :: IO ()
main = do
    tweets <- collectTweets 500
    return ()
    -- let text = T.unlines tweets
    -- markov <- markovStr (T.unpack text)
    -- poem <- findPoem [5,7,5,7,7] markov
    -- tweet $ T.pack poem
