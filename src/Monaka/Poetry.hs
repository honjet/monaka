module Monaka.Poetry
  ( findPoem
  ) where

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
                   } deriving (Eq, Show)

findPoem :: [Int] -> String -> IO String
findPoem sounds source = do
  mecab <- new ["mecab", "-l0"]
  nodes <- parseToNodes mecab source
  let poem = findMorphs sounds [x | (Just x) <- map toMorph nodes]
  case poem of
    Nothing     -> return "";
    Just morphs -> return $ unlines $ map fromMorphs morphs

fromMorphs :: [Morph] -> String
fromMorphs = intercalate "" . map word

findMorphs :: [Int] -> [Morph] -> Maybe [[Morph]]
findMorphs [] _ = return []
findMorphs (x:xs) ys = do
  morphs <- scrapeSounds x ys
  return $ fst morphs : fromMaybe [] (findMorphs xs $ snd morphs)

-- Maybe (抜き出したMorphs、 抜き出した語句より後ろのMorphs)
scrapeSounds :: Int -> [Morph] -> Maybe ([Morph], [Morph])
scrapeSounds _ [] = Nothing
scrapeSounds n xs = case peekSounds n xs of
  Nothing -> scrapeSounds n (tail xs)
  Just ys -> if isRightWords ys
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
canBeHead node = hinshi && hinshi1 -- && surface
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
      "数" -> case ext !! 6 of
          "万" -> False
          "億" -> False
          "兆" -> False
          _   -> True
      "自立" -> case ext !! 6 of
          "する"  -> False
          "できる" -> False
          _     -> True
      _ -> True

    -- 文字チェック
    --surface =  any (`notElem` ignoreLetters) $ nodeSurface node

canBeLast :: Node String -> Bool
canBeLast node = hinshi && hinshi1 && katsuyou2 && surface
  where
    -- Node 分解
    ext = extractNode node

    -- 品詞チェック
    hinshi = case ext !! 0 of
      _ -> True

    -- 品詞細分類1 チェック
    hinshi1 = case ext !! 1 of
      "名詞接続" -> False
      "動詞接続" -> False
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
      "未然ウ接続" -> False
      "未然レル接続" -> False
      "ガル接続"   -> False
      "連用形" -> case ext !! 6 of
          "いる" -> False
          "ます" -> False
          "です" -> False
          _    -> False
      _        -> True

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

wrongNode :: Node String -> Bool
wrongNode node = isKigou || isKaomoji || isSilence
  where
    ext = extractNode node

    isKigou = case ext !! 6 of
      "＋" -> True
      "×" -> True
      "÷" -> True
      _   -> False

    isKaomoji = case head ext of
      "記号" -> case last ext of
          "カオモジ" -> True
          _      -> False
      _ -> False

    isSilence = case last ext of
      "サイレンス" -> case ext !! 6 of
          "…"  -> True
          "…。" -> True
          _    -> False
      _ -> False


toMorph :: Node String -> Maybe Morph
toMorph node
  | wrongNode node = Nothing
  | otherwise = let ext = extractNode node
                in Just Morph { word = nodeSurface node
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
