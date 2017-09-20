module Monaka.Poem
    ( compose
    ) where

import           Control.Monad (join)
import           Data.List     (intercalate)
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust, fromMaybe, isNothing)
import           Prelude       hiding (Word)
import           System.Random (randomRIO)
import           Text.MeCab

data Word = Begin | Middle String | End deriving (Eq, Show)

compose :: String -> IO String
compose source = do
  mecab <- new ["mecab", "-l0"]
  nodeLines <- mapM (parseToNodes mecab) (lines source)
  let wordLines = map (filter (not . null) . map nodeSurface) nodeLines
  let allWords = intercalate ["\n"] wordLines
  let table = generateTable 3 (toWords allWords)
  begin <- initialWords table
  case begin of
    Nothing -> return ""
    Just ws -> chainWords table (tail ws) $ fromWords ws

fromWord :: Word -> String
fromWord (Middle x) = x
fromWord _          = ""

fromWords :: [Word] -> String
fromWords = concatMap fromWord

toWords :: [String] -> [Word]
toWords [] = []
toWords (_:xs) = Begin : init (convert xs)
  where
    convert :: [String] -> [Word]
    convert [] = []
    convert (y:ys)
      | y == "\n" = End : Begin : convert ys
      | otherwise = Middle y : convert ys

sample :: [a] -> IO (Maybe a)
sample [] = return Nothing
sample xs = return <$> randomOne xs
  where
    randomOne ys = (ys !!) <$> randomRIO (0, length ys - 1)

generateTable :: Int -> [Word] -> [[Word]]
generateTable _ [] = []
generateTable n xxs@(_:xs)
  | length xxs < n = []
  | otherwise = (take n xxs) : generateTable n xs

initialWords :: [[Word]] -> IO (Maybe [Word])
initialWords table = sample (filter (\xs -> head xs == Begin) table)

nextWords :: [[Word]] -> [Word] -> IO (Maybe [Word])
nextWords _ []         = return Nothing
nextWords table prefix = sample (filter (\xs -> init xs == prefix) table)

chainWords :: [[Word]] -> [Word] -> String -> IO String
chainWords table prefix chain = do
  next <- nextWords table prefix
  if last prefix == End
  then return chain
  else case next of
    Nothing -> return chain
    Just ws -> chainWords table (tail ws) (chain ++ maybe "" fromWord (last <$> next))
