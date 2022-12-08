#!/usr/bin/env runhaskell

import Data.List (singleton)


-- +------+
-- | Main |
-- +------+

data Hand = R | P | S deriving (Enum, Eq, Show)

instance Read Hand where
  readsPrec _ (x:xs)
    | x == 'A' || x == 'X' = [(R, xs)]
    | x == 'B' || x == 'Y' = [(P, xs)]
    | x == 'C' || x == 'Z' = [(S, xs)]
  readsPrec _ _ = []

instance Ord Hand where
  R <= P = True
  S <= R = True
  P <= S = True
  _ <= _ = False

points :: Hand -> Ordering -> Int
points y o = (fromEnum y) + case o of
                              LT -> 7
                              EQ -> 4
                              GT -> 1

main :: IO ()
main = do
  input <- readFile "02.input"
  print . sum . map points1 . parse1  $ input
  print . sum . map points2 . parse2  $ input


-- +--------+
-- | Part 1 |
-- +--------+

-- | Parse a single line of the input.  Each line is expected to end
-- with a newline character.
parse1 :: String -> [(Hand, Hand)]
parse1 [] = []
parse1 (a:' ':b:'\n':xs) = (f a, f b) : parse1 xs
  where f = read . singleton

points1 :: (Hand, Hand) -> Int
points1 (x, y) = points y $ x `compare` y


-- +--------+
-- | Part 2 |
-- +--------+

parse2 :: String -> [(Hand, Ordering)]
parse2 [] = []
parse2 (a:' ':b:'\n':xs) = (read $ singleton a, x) : parse2 xs
  where x = case b of
              'X' -> GT
              'Y' -> EQ
              'Z' -> LT

points2 :: (Hand, Ordering) -> Int
points2 (x, o) = points y $ x `compare` y
  where f y = x `compare` y == o
        y = head $ filter f $ enumFrom R
