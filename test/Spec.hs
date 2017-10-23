import qualified Data.Text as T
import qualified Data.Text.IO  as TIO
import Monaka.Markov (markovChain)
import Monaka.Poetry (findPoem)
import Web.Twitter (collectTweets)

main :: IO ()
main = do
    tweets <- collectTweets 500
    let text = T.unlines tweets
    markov <- markovChain (T.unpack text)
    poem <- findPoem [5,7,5,7,7] markov
    TIO.putStrLn text
    putStrLn $ markov ++ "\n"
    putStrLn poem