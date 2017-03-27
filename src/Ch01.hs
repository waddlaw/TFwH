module Ch01
  ( commonWords, sortWords, countRuns, sortRuns, showRun
  , convert, convert1, convert2, convert2'
  ) where

import Prelude hiding (Word)
import Data.Char (toLower)
import Data.Ord (comparing)
import Data.List (sort, sortBy)

-- 1.3 例題: 頻出単語
type Text = [Char]
type Word = [Char]

-- |
--
-- 文章中の単語数を返す
--
commonWords :: Int -> Text -> String
commonWords n = concat . map showRun . take n
              . sortRuns . countRuns . sortWords
              . words . map toLower

-- |
--
-- 単語のリストをアルファベット順にソートする
--
-- >>> sortWords ["to", "be", "or", "not", "to", "be"]
-- ["be", "be", "not", "or", "to", "to"]
sortWords :: [Word] -> [Word]
sortWords = sort

-- |
--
-- 単語が何回連続して出現するかを数える
--
-- >>> countRuns ["be", "be", "not", "or", "to", "to"]
-- [(2, "be"), (1, "not"), (1, "or"), (2, "to")]
countRuns :: [Word] -> [(Int, Word)]
countRuns = foldr go []
  where
    go w [] = [(1, w)]
    go w1 (l@(n, w2):ls) = if w1 == w2 then (n+1, w2):ls else (1, w1):l:ls

-- |
--
-- 単語を頻度の降順でソートする
--
-- >>> sortRuns [(2, "be"), (1, "not"), (1, "or"), (2, "to")]
-- [(2, "be"), (2, "to"), (1, "not"), (1, "or")]
sortRuns :: [(Int, Word)] -> [(Int, Word)]
sortRuns = sortBy (flip $ comparing fst)

showRun :: (Int, Word) -> String
showRun (n, w) = mconcat [" ", w, ": " , show n, "\n"]

-- 1.4 例題:数を言葉に変換する
-- |
--
-- 数を言葉に変換する
--
-- >>> convert 308000
-- "three hundred and eight thousand"
convert :: Int -> String
convert = convert6

units, teens, tens :: [String]
units = [ "zero", "one", "two", "three", "four", "five"
        , "six", "seven", "eight", "nine"
        ]
teens = [ "ten", "eleven", "twelve", "thirteen", "fourteen"
        , "fifteen", "sixteen", "seventeen", "eighteen"
        , "nineteen"
        ]
tens  = [ "twenty", "thirty", "forty", "fifty", "sixty"
        , "seventy", "eighty", "ninety"
        ]

-- |
--
-- 与えらる数値が1桁 (0 <= n < 10) の場合
--
convert1 :: Int -> String
convert1 n = units !! n

-- |
--
-- 2桁の数値のそれぞれの値を求める
-- 実際の定義では利用されない
digits2 :: Int -> (Int, Int)
digits2 n = (n `div` 10, n `mod` 10)

convert2 :: Int -> String
convert2 = combine2 . digits2

combine2 :: (Int, Int) -> String
combine2 (t, u)
  | t == 0           = units !! u
  | t == 1           = teens !! u
  | 2 <= t && u == 0 = tens !! (t - 2)
  | otherwise = tens !! (t - 2) ++ "-" ++ units !! u

convert2' :: Int -> String
convert2' n
  | t == 0 = units !! u
  | t == 1 = teens !! u
  | u == 0 = tens !! (t - 2)
  | otherwise = tens !! (t - 2) ++ "-" ++ units !! u
  where
    (t, u) = (n `div` 10, n `mod` 10)

convert3 :: Int -> String
convert3 n
  | h == 0    = convert2' t
  | t == 0    = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundred and " ++ convert2' t
  where
    (h, t) = (n `div` 100, n `mod` 100)

convert6 :: Int -> String
convert6 n
  | m == 0    = convert3 h
  | h == 0    = convert3 m ++ " thousand"
  | otherwise = convert3 m ++ " thousand" ++ link h ++ convert3 h
  where
    (m, h) = (n `div` 1000, n `mod` 1000)

link :: Int -> String
link h = if h < 100 then " and " else " "
