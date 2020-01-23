module Lib
  ( factorials
  , perm
  ) where

-- |Return the reversed list of all factorials mapped over the the
--  range [0; n-1]
-- factorials :: (Ord a, Num a, Enum a) => a -> [a]
factorials :: (Num a, Enum a) => a -> [a]
factorials n = scanr (*) 1 [n-1, n-2 .. 1]

-- |Remove an element by index from the given list
pop :: Int -> [a] -> [a]
pop i xs = (take i xs) ++ (drop (i + 1) xs)

-- |Return the nth permutation of xs
perm :: Int -> [a] -> [a]
perm index xs = let (_, _, ys) = perm' index xs in reverse ys

-- The memo in f is made up of the following triplet:
--   * remainder of the index
--   * elements left to pick from
--   * picked elements
perm' :: Int -> [a] -> (Int, [a], [a])
perm' index xs = foldl f (index, xs, []) (factorials $ length xs)
  where f (n, elements, picked) fact =
          let q = n `quot` fact -- index/factorial quotient
              r = n `rem` fact  -- index/factorial remainder
              p = elements !! q -- picked element
          in (r, pop q elements, p:picked)
