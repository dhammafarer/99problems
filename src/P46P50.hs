module P46P50 where

import Data.Foldable (traverse_)

-------------------------------------------------------------------------------

not' :: Bool -> Bool
not' True  = False
not' False = True

and', or', nor', nand', xor', imp', equ' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' False False = False
or' _ _         = True

xor' True False = True
xor' False True = True
xor' _ _        = False

nor' x y = not' $ or' x y

nand' x y = not $ and' x y

imp' True False = False
imp' _ _ = True

equ' True True = True
equ' False False = True
equ' _ _ = False

-------------------------------------------------------------------------------

table :: (Bool -> Bool -> Bool) -> IO ()
table f = traverse_ putStrLn t
  where t = [show a ++ " " ++ show b ++ " " ++ show (f a b) |
              a <- [True, False], b <- [True, False]]
