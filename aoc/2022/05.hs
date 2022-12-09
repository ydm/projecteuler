#!/usr/bin/env runhaskell

type Crate = Char
type Stack = [Crate] -- String
type Cargo = [Stack]

data Procedure = Procedure { quantity  :: Int
                           , fromStack :: Int
                           , toStack   :: Int } deriving Show


instance Read Procedure where
  readsPrec _ s = [(p, "")]
    where xs = words s
          p = Procedure { quantity  = read (xs!!1)
                        , fromStack = read (xs!!3) - 1
                        , toStack   = read (xs!!5) - 1 }

-- [T]     [D]         [L]            
-- [R]     [S] [G]     [P]         [H]
-- [G]     [H] [W]     [R] [L]     [P]
-- [W]     [G] [F] [H] [S] [M]     [L]
-- [Q]     [V] [B] [J] [H] [N] [R] [N]
-- [M] [R] [R] [P] [M] [T] [H] [Q] [C]
-- [F] [F] [Z] [H] [S] [Z] [T] [D] [S]
-- [P] [H] [P] [Q] [P] [M] [P] [F] [D]
initial :: Cargo
initial = [ "TRGWQMFP"
          , "RFH"
          , "DSHGVRZP"
          , "GWFBPHQ"
          , "HJMSP"
          , "LPRSHTZM"
          , "LMNHTP"
          , "RQDF"
          , "HPLNCSD" ]
-- initial = [ "NZ"
--           , "DCM"
--           , "P" ]
replace :: [a] -> Int -> a -> [a]
replace xs index z = a ++ z:b
  where (a,_:b) = splitAt index xs

move :: Procedure -> Cargo -> Cargo
move procedure xs = zs
  where q = quantity procedure
        f = fromStack procedure
        t = toStack procedure
        (front, back) = splitAt q $ xs!!f
        ys = replace xs f back
        zs = replace ys t (reverse front ++ xs!!t)

main :: IO ()
main = do
  xs <- (map read . lines <$> readFile "05.input") :: IO [Procedure]
  print $ map f $ foldr move initial xs
    where f s = if null s
                then ' '
                else head s
  -- print $ foldr move initial xs
