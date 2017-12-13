module Lists2 where

import qualified Data.List as L

-------------------------------------------------------------------------------

data Multilist a = Single a | Multiple Int a deriving Show

encodeMod :: Eq a => [a] -> [Multilist a]
encodeMod = map f . L.group
  where f [x] = Single x
        f xs  = Multiple (length xs) (head xs)

-------------------------------------------------------------------------------

decodeMod :: [Multilist a] -> [a]
decodeMod = concatMap f
  where
    f (Multiple n c) = replicate n c
    f (Single c)     = [c]

-------------------------------------------------------------------------------

encodeDirect :: Eq a => [a] -> [Multilist a]
encodeDirect = foldr f []
  where
    f x [] = [Single x]
    f x x'@(Single y : ys)
      | x == y    = Multiple 2 x : ys
      | otherwise = Single x : x'
    f x x'@(Multiple n y : ys)
      | x == y    = Multiple (n+1) x : ys
      | otherwise = Single x : x'

-------------------------------------------------------------------------------

