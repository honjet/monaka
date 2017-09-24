import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Monaka.Haiku
import           Monaka.Poem
import           Text.MeCab

main :: IO ()
main = do
  putStrLn =<< version

  putStrLn =<< find57577 "Haskell は高階関数や静的多相型付け、定義可能な演算子、例外処理といった多くの言語で採用されている現代的な機能に加え、パターンマッチングやカリー化、リスト内包表記、ガードといった多くの特徴的な機能を持っている。また、遅延評価や再帰的な関数や代数的データ型もサポートしているほか、独自の概念として圏論のアイデアを利用し参照透過性を壊すことなく副作用のある操作（例えば 代入、入出力、配列など）を実現するモナドを含む。このような機能の組み合わせにより、手続き型プログラミング言語では記述が複雑になるような処理がしばしば簡潔になるばかりではなく、必要に応じて手続き型プログラミングを利用できる。"
