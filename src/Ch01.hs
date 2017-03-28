{-|
Module      : Ch01
Description : 第1章 関数プログラミングとは何か
Copyright   : (c) Shinya Yamaguchi, 2017
Stability   : experimental
Portability : POSIX
-}
module Ch01 (
  -- * 1.3 例題: 頻出単語 (p.17-p.20)
  -- ** 型
  Text, Word
  -- ** 関数
  , commonWords, sortWords, countRuns, sortRuns, showRun
  -- * 1.4 例題: 数を言葉に変換する (p.20-p.25)
  -- ** 関数
  , convert, convert1, digits2, convert2, combine2, convert2', convert3, convert6, link
  -- * 1.6 練習問題
  -- ** 練習問題A
  , double
  -- ** 練習問題B
  , ansB
  -- ** 練習問題C
  , ansC
  -- ** 練習問題D
  , ansD
  -- ** 練習問題E
  , ansE
  -- ** 練習問題F
  -- *** 型
  , Label
  -- *** 関数
  , anagrams, showEntry
  ) where

import Prelude hiding (Word)
import Data.Char (toLower)
import Data.Ord (comparing)
import Data.List (sort, sortBy, sortOn, intersperse)

-- 1.3 例題: 頻出単語
-- | 文章を表す型
type Text = [Char]
-- | 単語を表す型
type Word = [Char]

-- |
--
-- 文章中の単語数を返す
--
-- >>> showRun (2, "be")
-- " be: 2\n"
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
-- ["be","be","not","or","to","to"]
--
sortWords :: [Word] -> [Word]
sortWords = sort

-- |
--
-- 単語が何回連続して出現するかを数える
--
-- >>> countRuns ["be", "be", "not", "or", "to", "to"]
-- [(2,"be"),(1,"not"),(1,"or"),(2,"to")]
--
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
-- [(2,"be"),(2,"to"),(1,"not"),(1,"or")]
--
sortRuns :: [(Int, Word)] -> [(Int, Word)]
sortRuns = sortBy (flip $ comparing fst)

-- |
--
-- 結果を整形する
--
-- >>> showRun (2, "be")
-- " be: 2\n"
--
showRun :: (Int, Word) -> String
showRun (n, w) = mconcat [" ", w, ": " , show n, "\n"]

-- 1.4 例題:数を言葉に変換する
-- |
--
-- 数を言葉に変換する
--
-- >>> convert 308000
-- "three hundred and eight thousand"
--
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
-- 与えられる数値が1桁 (0 <= n < 10) の場合
--
convert1 :: Int -> String
convert1 n = units !! n

-- |
--
-- 2桁の数値のそれぞれの値を求める
--
-- __定義のみで利用しない__
--
digits2 :: Int -> (Int, Int)
digits2 n = (n `div` 10, n `mod` 10)

-- |
--
-- ver.1 与えられる数値が2桁 (10 <= n < 100) の場合
--
-- __定義のみで利用しない__
--
convert2 :: Int -> String
convert2 = combine2 . digits2

-- |
--
-- 2つの数値を繋げて言葉に変換する
--
-- __定義のみで利用しない__
--
combine2 :: (Int, Int) -> String
combine2 (t, u)
  | t == 0           = units !! u
  | t == 1           = teens !! u
  | 2 <= t && u == 0 = tens !! (t - 2)
  | otherwise = tens !! (t - 2) ++ "-" ++ units !! u

-- |
--
-- ver.2 与えられる数値が2桁 (10 <= n < 100) の場合
--
convert2' :: Int -> String
convert2' n
  | t == 0 = units !! u
  | t == 1 = teens !! u
  | u == 0 = tens !! (t - 2)
  | otherwise = tens !! (t - 2) ++ "-" ++ units !! u
  where
    (t, u) = (n `div` 10, n `mod` 10)

-- |
--
-- 3桁までの数を言葉に変換する
--
convert3 :: Int -> String
convert3 n
  | h == 0    = convert2' t
  | t == 0    = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundred and " ++ convert2' t
  where
    (h, t) = (n `div` 100, n `mod` 100)

-- |
--
-- 6桁までの数を言葉に変換する
--
convert6 :: Int -> String
convert6 n
  | m == 0    = convert3 h
  | h == 0    = convert3 m ++ " thousand"
  | otherwise = convert3 m ++ " thousand" ++ link h ++ convert3 h
  where
    (m, h) = (n `div` 1000, n `mod` 1000)

-- |
--
-- 0 < m かつ 0 < h < 100 の場合に and でつなぐ関数
--
-- > (m, h) = (n `div` 1000, n `mod` 1000)
--
link :: Int -> String
link h = if h < 100 then " and " else " "

-- 1.6 練習問題
-- |
--
-- 整数を2倍する関数
--
-- >>> map double [1,4,4,3]
-- [2,8,8,6]
-- >>> map (double . double) [1,4,4,3]
-- [4,16,16,12]
-- >>> map double []
-- []
--
-- > sum    :: [Integer] -> Integer
-- > map    :: (a -> b) -> [a] -> [b]
-- > concat :: [[a]] -> [a]
-- > sort   :: Ord a => [a] -> [a]
--
-- 以下の性質が成り立つ
--
-- prop> (sum $ map double xs) == (double $ sum xs)
-- prop> (sum $ map sum xs) == (sum $ concat xs)
-- prop> (sum $ sort xs) == (sum xs)
--
double :: Integer -> Integer
double = (2*)

-- |
--
-- =正しい Haskell の式はどれか？
--
-- == 正しい
-- > sin theta^2
-- > (sin theta)^2
-- == 間違い
-- > sin^2 theta
--
-- == 問題の答え
-- > sin (theta*2) / (2*pi)
--
-- == 以下は結合性により、求める結果とならないことに注意
-- > sin (2 * theta) / 2 * pi
--
ansB :: String
ansB = undefined

-- |
--
-- = 問題の答え
-- > 'H' :: Char
-- > "H" :: String
--
-- 2001は数値であり、"2001"は文字列。
--
-- >>> [1,2,3] ++ [3,2,1]
-- [1,2,3,3,2,1]
--
-- >>> "Hello" ++ " World!"
-- "Hello World!"
--
-- >>> [1,2,3] ++ []
-- [1,2,3]
--
-- >>> "Hello" ++ "" ++ "World!"
-- "HelloWorld!"
ansC :: String
ansC = undefined


-- |
--
-- = 問題の答え
--
-- すべてのテキスト中の文字をすべて小文字に変換してから、テキストを単語に分ける方法
-- > words . map toLower
--
-- 単語に分けてから小文字に変換する方法
-- 
-- > map (map toLower) . words
-- > words   :: String -> [String]
-- > toLower :: Char -> Char
--
-- prop> (words $ map toLower xs) == (map (map toLower) $ words xs)
ansD :: String
ansD = undefined

-- |
--
-- = 問題の答え
--
-- (+)  : 結合性を持つ (単位元: 0)
--
-- (++) : 結合性を持つ (単位元: [])
--
-- (.)  : 結合性を持つ (単位元: id)
--
-- (/)  : 結合性を持たない
--
-- prop> (x + (y + z)) == ((x + y) + z)
-- prop> (xs ++ (ys ++ zs)) == ((xs ++ ys) ++ zs)
ansE :: String
ansE = undefined

-- |
--
-- anagrams s はアルファベット順の英単語のリストを取り、n文字の単語だけを取り出し、文字列を生成する
--
anagrams :: Int -> [Word] -> String
anagrams n = concat . map showEntry . groupByLabel
           . sortLabels . map addLabel . getWords n

-- |
--
-- 長さ n の単語を取り出す
--
-- >>> getWords 2 ["abc", "d", "efg", "hi", "j", "", "kl"]
-- ["hi","kl"]
getWords :: Int -> [Word] -> [Word]
getWords n = filter ((==n) . length)

-- | アナグラムのラベル
type Label = [Char]

-- |
--
-- ラベルを付ける関数 (ラベルは最初と最後を入れ替えた形式)
--
-- >>> addLabel "word"
-- ("dorw","word")
--
addLabel :: Word -> (Label, Word)
addLabel w = (l, w)
  where
   hw = head w
   lw = last w
   mw = tail $ init w
   l = [lw] ++ mw ++ [hw]

-- |
--
-- ラベル付きの単語のリストをラベルのアルファベット順にソートする
--
-- >>> sortLabels [("dorw", "word"), ("abcd", "dbca")]
-- [("abcd","dbca"),("dorw","word")]
--
sortLabels :: [(Label, Word)] -> [(Label, Word)]
sortLabels = sortOn fst

-- |
--
-- 同じラベルの単語をまとめる
--
-- >>> groupByLabel [("dorw", "zzzz"), ("dorw", "eeee")]
-- [("dorw",["eeee","zzzz"])]
--
groupByLabel :: [(Label, Word)] -> [(Label, [Word])]
groupByLabel = init . foldl go e
  where
    e = [("", [])]
    go [] _ = []
    go list@((l1, w1):xs) (l2, w2) = if l1 == l2 then (l1, sort $ w2:w1):xs else (l2, [w2]):list

-- |
--
-- 対応表を文字列に変換し、連結する
--
-- >>> showEntry ("eginor", ["ignore", "region"])
-- "eginor: ignore,region"
showEntry :: (Label, [Word]) -> String
showEntry (l, ws) = l ++ ": " ++ (concat $ intersperse "," ws)
