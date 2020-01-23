module Main where

import Lib

ans :: [Int]
ans = perm 999999 [0..9]

main :: IO ()
main = putStrLn $ concat $ map show ans
