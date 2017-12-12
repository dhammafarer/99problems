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
