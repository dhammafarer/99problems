module Lists1 where

myLast :: [a] -> a
myLast = foldr1 (flip const)

myLastSafe :: [a] -> Maybe a
myLastSafe [] = Nothing
myLastSafe xs = Just $ foldr1 (flip const) xs

myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [x] = Nothing
myButLast [x,_] = Just x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> Maybe a
elementAt _ 0 = Nothing
elementAt [] _ = Nothing
elementAt (x:_) 1 = Just x
elementAt (_:xs) n = elementAt xs (n - 1)

myLength :: [a] -> Int
myLength = foldr (const (+1)) 0

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == ys
  where ys = myReverse xs

-- myFlatten --
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List xs) = foldr ((++) . myFlatten) [] xs
---------------
