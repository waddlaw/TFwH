module Ch01
  ( commonWords, sortWords, countRuns, sortRuns, showRun
  , convert, convert1
  ) where

import Prelude hiding (Word)
import Data.Char (toLower)
import Data.Ord (comparing)
import Data.List (sort, sortBy)

type Text = [Char]
type Word = [Char]

commonWords :: Int -> String -> String
commonWords n = concat . map showRun . take n
              . sortRuns . countRuns . sortWords
              . words . map toLower

sortWords :: [Word] -> [Word]
sortWords = sort

countRuns :: [Word] -> [(Int, Word)]
countRuns = foldr go []
  where
    go w [] = [(1, w)]
    go w1 (l@(n, w2):ls) = if w1 == w2 then (n+1, w2):ls else (1, w1):l:ls

sortRuns :: [(Int, Word)] -> [(Int, Word)]
sortRuns = sortBy (flip $ comparing fst)

showRun :: (Int, Word) -> String
showRun (n, w) = mconcat [" ", w, ": " , show n, "\n"]

-- 1.4
convert :: Int -> String
convert = undefined

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

convert1 :: Int -> String
convert1 n = units !! n

digits2 :: Int -> (Int, Int)
digits2 n = (n `div` 10, n `mod` 10)

convert2 :: Int -> String
convert2 = n
  | t == 0 = units !! u
  | t == 1 = teens !! u
  | u == 0 = tens !! (t - 2)
  | otherwise = tens !! (t - 2) ++ "-" ++ units !! u
  where
    (t, u) = (n `div` 10, n `mod` 10)

combine2 :: Int -> String
combine2 n
  | t == 0         = units !! u
  | t == 1         = teens !! u
  | u == 0 = tens !! (t - 2)
  | otherwise = tens !! (t - 2) ++ "-" ++ units !! u
  where
    (t, u) = (n `div` 10, n `mod` 10)
