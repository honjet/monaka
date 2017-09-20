module Monaka.Haiku where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.MeCab

data Morph = Morph { word     :: String
                   , yomi     :: String
                   , len      :: Int
                   , headable :: Bool
                   , lastable :: Bool
                   } deriving (Show)

find575 :: String -> IO String
find575 source = do
  -- TODO: 辞書
  mecab <- new ["mecab", "-l0 -d /usr/lib/mecab/dic/mecab-ipadic-neologd/"]
  nodes <- parseToNodes mecab source
  let fsf = find575Morphs $ map toMorph nodes
  case fsf of
    Nothing       -> return ""
    (Just morphs) -> return $ unlines $ map toString morphs

find57577 :: String -> IO String
find57577 source = do
  -- TODO: 辞書
  mecab <- new ["mecab", "-l0 -d /usr/lib/mecab/dic/mecab-ipadic-neologd/"]
  nodes <- parseToNodes mecab source
  let fsf = find57577Morphs $ map toMorph nodes
  case fsf of
    Nothing       -> return ""
    (Just morphs) -> return $ unlines $ map toString morphs

toString :: [Morph] -> String
--toString xs = filter (`notElem` ignoreLetters) $ unwords $ map word xs
toString xs = [x | x <- unwords $ map word xs, x `notElem` " " ]

find575Morphs :: [Morph] -> Maybe [[Morph]]
find575Morphs xs = do
  first <- scrapeSounds 5 xs
  second <- scrapeSounds 7 (snd first)
  third <- scrapeSounds 5 (snd second)
  return [fst first, fst second, fst third]

find57577Morphs :: [Morph] -> Maybe [[Morph]]
find57577Morphs xs = do
  first <- scrapeSounds 5 xs
  second <- scrapeSounds 7 (snd first)
  third <- scrapeSounds 5 (snd second)
  forth <- scrapeSounds 7 (snd third)
  fifth <- scrapeSounds 7 (snd forth)
  return [fst first, fst second, fst third, fst forth, fst fifth]

-- Maybe (抜き出したMorphs、 抜き出した語句より後ろのMorphs)
scrapeSounds :: Int -> [Morph] -> Maybe ([Morph], [Morph])
scrapeSounds _ [] = Nothing
scrapeSounds n xs = case peekSounds n xs of
  Nothing   -> scrapeSounds n (tail xs)
  (Just ys) -> if isRightWords ys
               then return (ys, drop (length ys) xs)
               else scrapeSounds n (tail xs)

isRightWords :: [Morph] -> Bool
isRightWords morphs = headable (head morphs) && lastable (last morphs)

peekSounds :: Int -> [Morph] -> Maybe [Morph]
peekSounds _ [] = Nothing
peekSounds n (x:xs)
  | len x > n = Nothing
  | len x == 0 = Nothing
  | len x == n = return [x]
  | otherwise = (:) <$> Just x <*> peekSounds (n - len x) xs

countMora :: String -> Int
countMora xs = length $ filter (`elem` mora) xs
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

-- 0: 品詞
-- 1: 品詞細分類1
-- 2: 品詞細分類2
-- 3: 品詞細分類3
-- 4: 活用形1
-- 5: 活用形2
-- 6: 原形
-- 7: 読み
-- 8: 発音
extractNode :: Node String -> [String]
extractNode node = splitOn "," (nodeFeature node)

canBeHead :: Node String -> Bool
canBeHead node = hinshi && hinshi1 && surface
  where
    -- Node 分解
    ext = extractNode node

    -- 品詞チェック
    hinshi = case ext !! 0 of
      "助詞"   -> False
      "助動詞"  -> False
      "フィラー" -> False
      _      -> True

    -- 品詞細分類1 チェック
    hinshi1 = case ext !! 1 of
      "接尾" -> False
      "非自立" -> False
      "自立" -> case ext !! 6 of
          "する"  -> False
          "できる" -> False
          _     -> True
      _ -> True

    -- 文字チェック
    surface =  any (`notElem` ignoreLetters) $ nodeSurface node

canBeLast :: Node String -> Bool
canBeLast node = hinshi && hinshi1 && katsuyou2
  where
    -- Node 分解
    ext = extractNode node

    -- 品詞チェック
    hinshi = case ext !! 0 of
      "助詞" -> False
      "助動詞" -> case ext !! 6 of
          "だ" -> False
          _   -> True
      _ -> True

    -- 品詞細分類1 チェック
    hinshi1 = case ext !! 1 of
      "名詞接続" -> False
      "格助詞"  -> False
      "係助詞"  -> False
      "連体化"  -> False
      "接続助詞" -> False
      "並立助詞" -> False
      "副詞化"  -> False
      "数接続"  -> False
      "非自立"  -> case ext !! 5 of
          "連用形" -> False
          _     -> True
      _      -> True

    --活用形2 チェック
    katsuyou2 = case ext !! 5 of
      "未然形"    -> False
      "仮定形"    -> False
      "連用タ接続"  -> False
      "未然レル接続" -> False
      "ガル接続"   -> False
      _        -> True

toMorph :: Node String -> Morph
toMorph node = let ext = extractNode node
                 in Morph { word = nodeSurface node
                          , yomi = last ext
                          , len = countMora (last ext)
                          , headable = canBeHead node
                          , lastable = canBeLast node
                          }

ignoreLetters :: [Char]
ignoreLetters = ['！' , '？' , '＠' , '＃' , '＄'
                , '％' , '＾' , '＊' , '＿' , '＝'
                , '＋' , '；' , '：' , '｜' , '。'
                , '、' , '・' , '　' , '（' , '）'
                , '「' , '」' , '＜' , '＞' , '('
                , ')' , '[' , ']' , '!' , '@'
                , '#' , '$' , '%' , '^' , '&'
                , '*' , '-' , '=' , '+' , '{'
                , '}' , ';' , ':' , ',' , '.'
                , '/' , '<' , '>' , '|' , '?'
                , '\'' , '\'' , '\\', '~', '`'
                , ' '
                ]
