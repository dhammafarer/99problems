module P21P28 where

import System.Random
import Control.Monad

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

diffSelect :: Int -> Int -> [Int]
diffSelect n m = undefined
