module P54P60 where

-------------------------------------------------------------------------------

data Tree a =
    Leaf
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq, Ord)

insert' :: Ord a => a -> Tree a -> Tree a
insert' x Leaf = Node x Leaf Leaf
insert' x t@(Node x' a b)
  | x == x' = t
  | x > x'  = Node x' Leaf (insert' x b)
  | x < x'  = Node x' (insert' x b) Leaf

map' :: (a -> b) -> Tree a -> Tree b
map' f Leaf = Leaf
map' f (Node x left right) = Node (f x) (map' f left) (map' f right)

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ b Leaf = b
foldTree f b (Node a left right) = foldTree f (foldTree f (f a b) left) right

-------------------------------------------------------------------------------

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Leaf]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
              in [Node 'x' left right | i <- [q..q+r],
                                        left <- cbalTree i,
                                        right <- cbalTree (n - i - 1)]

-------------------------------------------------------------------------------

symmetric :: Tree a -> Bool
symmetric = undefined
