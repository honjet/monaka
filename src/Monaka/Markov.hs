{-# LANGUAGE FlexibleInstances #-}

module Monaka.Markov
  ( markovChain
  ) where

import           Control.Monad
import           Data.List        (intercalate)
import qualified Data.MarkovChain as Markov
import           System.Random
import           Text.MeCab

instance Ord (Node String) where
  compare x y = compare (nodeSurface x) (nodeSurface y)

markovChain :: String -> IO String
markovChain source = do
  mecab <- new ["mecab", "-l0"]
  nodes <- parseToNodes mecab source
  nodes' <- markovNode nodes
  let sentence = map fromNode nodes'
  return $ intercalate "" sentence

markovNode :: [Node String] -> IO [Node String]
markovNode source = do
  rnd <- randomRIO (0, length source)
  gen <- newStdGen
  return $ take 200 $ Markov.run 3 source rnd gen

fromNode :: Node String -> String
fromNode node = filter (`notElem` ignoreLetters) $ nodeSurface node

ignoreLetters = [' ', '　']
