#!/usr/bin/env runhaskell

import Data.List (sort, span)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

split :: T.Text -> [T.Text]
split s = T.splitOn (T.pack "\n") s

spans :: [T.Text] -> [[T.Text]]
spans [] = []
spans xs = ys : (spans $ tail zs)
  where (ys, zs) = span pred xs
        empty    = T.pack ""
        pred     = (/= empty)

parse :: T.Text -> Int
parse = read . T.unpack

main :: IO ()
main =
  last                      <$>
  sort                      <$>
  map sum                   <$>
  map (map parse)           <$>
  spans                     <$>
  (map T.strip)             <$>
  split                     <$>
  (TIO.readFile "01.input") >>= (putStr . show)
