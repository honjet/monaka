module Main where

import Monaka.Poem
import Monaka.Haiku
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  source <- readFile $ head args
  --sentence <- compose source
  --putStrLn sentence
  putStrLn =<< find57577 source


