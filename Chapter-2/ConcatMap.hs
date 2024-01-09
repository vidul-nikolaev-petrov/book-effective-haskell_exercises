module Chapter2.ConcatMap where

concatMap' :: (a -> b) -> [[a]] -> [b]
concatMap' f = foldl (\acc xs -> foldr (:) [f x | x <- xs] acc) []

main :: IO ()
main = do
    print $ concatMap' (* 10) [[1, 2], [3, 4]]