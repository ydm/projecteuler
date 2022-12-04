#!/usr/bin/env runhaskell

import Data.Char
import qualified Data.CharSet as CharSet

group3 :: [String] -> [[String]]
group3 [] = []
group3 (a:b:c:xs) = [a, b, c] : group3 xs

interx :: [String] -> Char
interx xs = pick $ foldl CharSet.intersection y ys
  where (y:ys) = map CharSet.fromList xs

pick :: CharSet.CharSet -> Char
pick = head . CharSet.toList

points :: Char -> Int
points c = ord c - offset
  where offset =
          if isLower c
          then ord 'a' -  1
          else ord 'A' - 27

split :: String -> [String]
split s = f $ splitAt (length s `div` 2) s
  where f (a, b) = [a, b]

main :: IO ()
main = do
  input <- lines <$> readFile "03.input"
  print . sum . map (points . interx . split) $  input
  print . sum . (map (points . interx) . group3) $ input
