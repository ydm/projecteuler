#!/usr/bin/env runhaskell

import qualified Data.Set as S

-- TODO: Iterate over the string using a single set, and on each step
-- add the next element and remove the least recent one.  This way
-- I'll achieve O(n) instead of O(n logn).

findUnique :: Int -> String -> Int
findUnique target s =
  if S.size (S.fromList $ take target s) == target
  then 0
  else 1 + findUnique target (tail s)

one = (+ 4) . findUnique  4
two = (+14) . findUnique 14

main :: IO ()
main = do
  input <- readFile "06.input"
  print $ one input
  print $ two input
