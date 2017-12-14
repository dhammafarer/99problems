module P11P20 where

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

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)
dupli' xs = xs >>= replicate 2

-------------------------------------------------------------------------------

repli, repli' :: [a] -> Int -> [a]
repli xs n = xs >>= f n
  where f 0 _ = []
        f 1 x = [x]
        f n x = x : f (n-1) x

repli' xs n = xs >>= replicate n

-------------------------------------------------------------------------------

dropEvery :: [a] -> Int -> [a]
dropEvery _ 0 = []
dropEvery _ 1 = []
dropEvery xs n = f xs n
  where f [] _     = []
        f (x:xs) 1 = f xs n
        f (x:xs) n = x : f xs (n-1)

-------------------------------------------------------------------------------

split :: [a] -> Int -> ([a], [a])
split [] _ = ([],[])
split xs n = (,) (f n xs) (s n xs)
  where f _ [] = []
        f 0 xs = []
        f n (x:xs) = x : f (n-1) xs
        s _ [] = []
        s 0 xs = xs
        s n (x:xs) = s (n-1) xs

-------------------------------------------------------------------------------

slice, slice' :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i-1) $ take k xs

slice' xs i k = map snd
              $ filter (\(x,_) -> x >= 1 && x <= k)
              $ zip [1..] xs

-------------------------------------------------------------------------------

rotate, rotate' :: [a] -> Int -> [a]
rotate [] _    = []
rotate xs n
  | n == 0     = xs
  | n < 0     = drop n' xs ++ take n' xs
  | otherwise = drop n xs ++ take n xs
    where n' = length xs + n

rotate' xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs

-------------------------------------------------------------------------------

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (x, ys' ++ xs')
  where
    (ys', x:xs') = splitAt (n-1) xs
