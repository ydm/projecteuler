module Main where

import Data.List (intercalate)
import Lib

ans = perm 999999 [0..9]
main = putStrLn $ intercalate "" $ map show ans
