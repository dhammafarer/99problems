module Main where

import Criterion.Main
import P21P28 (range, range')

myList :: [Int]
myList = [1..9999]

i :: Int
i = 1

k :: Int
k = 7000

main = defaultMain
  [ bench "range"
    $ whnf (range i) k
  , bench "range'"
    $ whnf (range' i) k
  ]
