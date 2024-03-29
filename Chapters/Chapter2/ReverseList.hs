module Chapter2.ReverseList where

reverseListL :: [a] -> [a]
reverseListL = foldl (flip (:)) []

-- much more inefficient because of the complexity
reverseListR :: [a] -> [a]
reverseListR = foldr (\x acc -> acc <> [x]) []
