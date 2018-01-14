{-# LANGUAGE OverloadedStrings #-}

module Monaka.Poetry
  ( PNode (..)
  , Feature (..)
  , findPoemInText
  , findPoemInNodes
  ) where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text       as T
import           Text.MeCab

data PNode = PNode { word      :: T.Text
                   , pron      :: T.Text
                   , mora      :: Int
                   , headable  :: Bool
                   , lastable  :: Bool
                   , mecabNode :: Node T.Text
                   } deriving (Eq, Show)

data Feature = Feature { hinshi   :: T.Text -- 品詞
                       , hinshi1  :: T.Text -- 品詞細分類1
                       , hinshi2  :: T.Text -- 品詞細分類2
                       , hinshi3  :: T.Text -- 品詞細分類3
                       , katsuyo1 :: T.Text -- 活用形1
                       , katsuyo2 :: T.Text -- 活用形2
                       , genkei   :: T.Text -- 原形
                       , yomi     :: T.Text -- 読み
                       , hatsuon  :: T.Text -- 発音
                       } | None


type PChunk = [PNode]

toPNode :: Node T.Text -> Maybe PNode
toPNode node
  | wrongNode node = Nothing
  | otherwise = let feature = extractNode node
                in Just PNode { word = nodeSurface node
                              , pron = hatsuon feature
                              , mora = countMora (hatsuon feature)
                              , headable = canBeHead node
                              , lastable = canBeLast node
                              , mecabNode = node
                              }

findPoemInText :: [Int] -> T.Text -> IO (Maybe T.Text)
findPoemInText sounds source = do
  mecab <- new ["mecab", "-l0"]
  nodes <- parseToNodes mecab source
  return $ findPoemInNodes sounds nodes

findPoemInNodes :: [Int] -> [Node T.Text] -> Maybe T.Text
findPoemInNodes sounds nodes =
  case splitChunk sounds [x | Just x <- map toPNode nodes] of
    Just poem
      | length poem == length sounds -> Just $ T.unlines $ map fromPChunk poem
      | otherwise -> Nothing
    _ -> Nothing

fromPChunk :: PChunk -> T.Text
fromPChunk = T.concat . map word

splitChunk :: [Int] -> PChunk -> Maybe [PChunk]
splitChunk [] _ = Nothing
splitChunk _ [] = Nothing
splitChunk [x] chunks = case scrapeChunk x chunks of
  Just (current, _)
    | okEnd (last current) -> Just [current]
    | otherwise -> splitChunk [x] (tail chunks)
  _ -> Nothing
splitChunk (x:xs) chunks = case scrapeChunk x chunks of
  Just (current, next) -> Just [current] <> splitChunk xs next
  _                    -> Nothing

-- Maybe (抜き出したPChunk、 抜き出した語句より後ろのPChunk)
scrapeChunk :: Int -> PChunk -> Maybe (PChunk, PChunk)
scrapeChunk _ [] = Nothing
scrapeChunk n chunks = case peekChunk n chunks of
  Just xs
    | okChunk xs -> return (xs, drop (length xs) chunks)
    | otherwise  -> scrapeChunk n (tail chunks)
  _ -> scrapeChunk n (tail chunks)

peekChunk :: Int -> PChunk -> Maybe PChunk
peekChunk _ [] = Nothing
peekChunk n (x:xs)
  | mora x > n = Nothing
  | mora x == n = return [x]
  | otherwise = (:) <$> Just x <*> peekChunk (n - mora x) xs

okChunk :: PChunk -> Bool
okChunk chunk = headable (head chunk) && lastable (last chunk)

okEnd :: PNode -> Bool
okEnd node = okHinshi || okHinshi1 || okKatsuyo2
  where
    feature = extractNode $ mecabNode node

    okHinshi = case hinshi feature of
      "名詞" -> case hinshi1 feature of
        "非自立" -> False
        _     -> True
      _ -> False

    okHinshi1 = case hinshi1 feature of
      h | T.isInfixOf "終助詞" h -> True
        | otherwise -> False

    okKatsuyo2 = case katsuyo2 feature of
      "基本形" -> True
      _     -> False

extractNode :: Node T.Text -> Feature
extractNode node
  | nodeStat node == NOR =
    let ext = T.splitOn "," (nodeFeature node)
    in Feature { hinshi   = ext !! 0 -- 品詞
               , hinshi1  = ext !! 1 -- 品詞細分類1
               , hinshi2  = ext !! 2 -- 品詞細分類2
               , hinshi3  = ext !! 3 -- 品詞細分類3
               , katsuyo1 = ext !! 4 -- 活用形1
               , katsuyo2 = ext !! 5 -- 活用形2
               , genkei   = ext !! 6 -- 原形
               , yomi     = ext !! 7 -- 読み
               , hatsuon  = ext !! 8 -- 発音
               }
  | otherwise = None

canBeHead :: Node T.Text -> Bool
canBeHead node = okHinshi && okHinshi1 -- && surface
  where
    feature = extractNode node

    okHinshi = case hinshi feature of
      "助詞"   -> False
      "助動詞"  -> False
      "フィラー" -> False
      "記号"   -> False
      _      -> True

    okHinshi1 = case hinshi1 feature of
      "接尾"   -> False
      "非自立" -> False
      "数" -> case genkei feature of
          "万" -> False
          "億" -> False
          "兆" -> False
          _   -> True
      "自立" -> case genkei feature of
          "する"  -> False
          "できる" -> False
          _     -> True
      _ -> True

canBeLast :: Node T.Text -> Bool
canBeLast node = okHinshi && okHinshi1 && okKatsuyo2 && surface
  where
    feature = extractNode node

    okHinshi = case hinshi feature of
      "名詞" -> case hinshi1 feature of
        "非自立" -> False
        _     -> True
      _ -> True

    okHinshi1 = case hinshi1 feature of
      "名詞接続" -> False
      "動詞接続" -> False
      "数接続"   -> False
      "非自立"   -> case katsuyo2 feature of
          "連用形" -> False
          _     -> True
      _      -> True

    okKatsuyo2 = case katsuyo2 feature of
      "未然形"       -> False
      "仮定形"       -> False
      "連用タ接続"   -> False
      "未然ウ接続"   -> False
      "未然レル接続" -> False
      "ガル接続"     -> False
      "連用形"       -> case genkei feature of
          "いる" -> False
          "ます" -> False
          "です" -> False
          _    -> False
      _ -> True

    surface = case nodeSurface node of
      "１" -> False
      "２" -> False
      "３" -> False
      "４" -> False
      "５" -> False
      "６" -> False
      "７" -> False
      "８" -> False
      "９" -> False
      "０" -> False
      _   -> True

wrongNode :: Node T.Text -> Bool
wrongNode node = isKigou || isKaomoji || isSilence || wrongWord
  where
    feature = extractNode node

    isKigou = case genkei feature of
      "＋" -> True
      "×" -> True
      "÷" -> True
      _   -> False

    isKaomoji = case hinshi feature of
      "記号" -> case hatsuon feature of
          "カオモジ" -> True
          _      -> False
      _ -> False

    isSilence = case hatsuon feature of
      "サイレンス" -> case genkei feature of
          "…"  -> True
          "…。" -> True
          _    -> False
      _ -> False

    wrongWord = case nodeSurface node of
      "殺す" -> True
      _    -> False

countMora :: T.Text -> Int
countMora xs = T.length $ T.filter (`elem` mora) xs
  where
    mora = ['ア','イ','ウ','エ','オ'
           ,'カ','キ','ク','ケ','コ'
           ,'ガ','ギ','グ','ゲ','ゴ'
           ,'サ','シ','ス','セ','ソ'
           ,'ザ','ジ','ズ','ゼ','ゾ'
           ,'タ','チ','ツ','テ','ト'
           ,'ッ'
           ,'ダ','ヂ','ヅ','デ','ド'
           ,'ナ','ニ','ヌ','ネ','ノ'
           ,'ハ','ヒ','フ','ヘ','ホ'
           ,'バ','ビ','ブ','ベ','ボ'
           ,'パ','ピ','プ','ペ','ポ'
           ,'マ','ミ','ム','メ','モ'
           ,'ヤ','ユ','ヨ'
           ,'ラ','リ','ル','レ','ロ'
           ,'ワ','ヲ','ン','ヴ','ー'
           ]
