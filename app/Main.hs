module Main where

import           Monaka.Markov
import           Monaka.Poetry      (findPoem)
import           System.Environment (getArgs)

main :: IO ()
main = getArgs
       >>= readFile <$> head
       >>= markovChain
       >>= putThorough
       >>= findPoem [5,7,5,7,7]
       >>= putStrLn

putThorough :: String -> IO String
putThorough src = do
  putStrLn $ src ++ "\n"
  return src
