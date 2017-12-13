module Lists2 where

import qualified Data.List as L

-------------------------------------------------------------------------------

data Multilist a = Single a | Multiple Int a deriving Show

encodeMod :: Eq a => [a] -> [Multilist a]
encodeMod = map f . L.group
  where f [x] = Single x
        f xs  = Multiple (length xs) (head xs)

-------------------------------------------------------------------------------
