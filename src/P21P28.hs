module P21P28 where

import System.Random
import Control.Monad
import qualified Data.List as L

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = xs' ++ x : ys'
  where
    (xs', ys') = splitAt (n-1) xs

-------------------------------------------------------------------------------

range, range' :: Int -> Int -> [Int]
range x y = x : f (x+1)
  where
    f x' | x' == y   = [y]
         | otherwise = x' : f (x'+1)

range' x y
    | x == y = [x]
    | x < y  = x : range' (x+1) y
    | x > y  = x : range' (x-1) y

-------------------------------------------------------------------------------

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n =
  let randomIdx = getStdRandom (randomR (0, length xs - 1))
      is = replicateM 3 randomIdx
   in fmap (fmap (xs !!)) is

-------------------------------------------------------------------------------

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = diffSelect' n [1..m]

diffSelect' :: Int -> [a] -> IO [a]
diffSelect' 0 _  = return []
diffSelect' _ [] = error "list is too short"
diffSelect' n xs = do r <- randomRIO (0, length xs - 1)
                      let ys = take r xs ++ drop (r+1) xs
                      rest <- diffSelect' (n-1) ys
                      return ((xs !! r) : rest)

-------------------------------------------------------------------------------

randPermu :: [a] -> IO [a]
randPermu xs = diffSelect' (length xs) xs

-------------------------------------------------------------------------------

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do
  y:xs' <- L.tails xs
  ys <- combinations (n-1) xs'
  return (y:ys)

tails :: [a] -> [[a]]
tails [] = []
tails xs = xs : tails (tail xs)

-------------------------------------------------------------------------------

lsort, lsort' :: [[a]] -> [[a]]
lsort = L.sortBy (\x y -> length x `compare` length y)

lsort' [] = []
lsort' xs = concat gs
  where gs = lsort . L.groupBy lenghts $ lsort xs
        lenghts x y = length x == length y
