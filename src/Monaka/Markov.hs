{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Monaka.Markov
  ( markovStr
  , markovNodes
  ) where

import           Control.Monad
import           Data.List        (intercalate)
import qualified Data.MarkovChain as Markov
import qualified Data.Text        as T
import           System.Random
import           Text.MeCab

instance Ord (Node T.Text) where
  compare x y = compare (nodeSurface x) (nodeSurface y)

markovStr :: T.Text -> IO T.Text
markovStr source = do
  nodes <- markovNodes source
  let text = map nodeSurface nodes
  return $ T.intercalate "" text

markovNodes :: T.Text -> IO [Node T.Text]
markovNodes source = do
  mecab <- new ["mecab", "-l0"]
  nodes <- parseToNodes mecab source
  let nodes' = filter ((`notElem` ignoreTexts) . nodeSurface) nodes
  rnd <- randomRIO (0, length nodes)
  gen <- newStdGen
  return $ take 300 $ Markov.run 3 nodes rnd gen

ignoreTexts :: [T.Text]
ignoreTexts = [" ", ",", "　", "、", "「", "」", "『", "』"]
