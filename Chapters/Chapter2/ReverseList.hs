module ReverseList where

reverseListL :: [a] -> [a]
reverseListL = foldl (flip (:)) []

-- much more inefficient because of the complexity
reverseListR :: [a] -> [a]
reverseListR = foldr (\x acc -> acc <> [x]) []

main :: IO ()
main = do
    print $ reverseListL [1 .. 3]
    print $ reverseListR [1 .. 3]