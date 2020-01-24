module Lib where

-- |Return the reversed list of all factorials mapped over the the
--  range [0; n-1]
-- factorials :: (Ord a, Num a, Enum a) => a -> [a]
factorials :: (Num a, Enum a) => a -> [a]
factorials n = scanr (*) 1 [n-1, n-2 .. 1]

remainders :: Integral a => a -> a -> [a]
remainders index len = scanl rem index $ factorials len

indices :: Integral a => a -> a -> [a]
indices index len = zipWith quot (remainders index len) (factorials len)

-- |Remove an element by index from the given list and return a tuple
--  of the removed element and the modified list
pop :: Int -> [a] -> (a, [a])
pop i xs = (pivot, left ++ right)
  where left = take i xs
        pivot:right = drop i xs

populate :: [a] -> [Int] -> [a]
populate _ [] = []
populate xs (i:is) = y : populate ys is
  where (y, ys) = pop i xs

perm :: Int -> [a] -> [a]
perm index xs = populate xs $ indices index $ length xs
