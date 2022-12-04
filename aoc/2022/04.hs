#!/usr/bin/env runhaskell

import Data.List
import Data.Maybe

splitOn :: Char -> String -> (String, String)
splitOn c s = splitAt (fromJust $ elemIndex c s) s

conv :: String -> (Int, Int)
conv s = (read a, read $ tail b)
  where (a, b) = splitOn '-' s

check1 :: ((Int, Int), (Int, Int)) -> Int
check1 ((a, b), (c, d)) = if a >= c && b <= d ||
                             c >= a && d <= b
                          then 1
                          else 0

check2 :: ((Int, Int), (Int, Int)) -> Int
check2 ((a, b), (c, d)) = if c <= a && a <= d ||
                             c <= b && b <= d ||
                             a <= c && c <= b ||
                             a <= d && d <= b
                          then 1
                          else 0

main :: IO ()
main = do
  input <- lines <$> readFile "04.input"
  print . sum . map (check1 . f . splitOn ',') $ input
  print . sum . map (check2 . f . splitOn ',') $ input
  where f (a, b) = (conv a, conv $ tail b)
