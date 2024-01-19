module Chapter2.ConcatMap where

concatMap' :: (a -> b) -> [[a]] -> [b]
concatMap' f = foldl (\acc xs -> foldr (:) (map f xs) acc) []

main :: IO ()
main = do
    print $ concatMap' (* 10) [[1, 2], [3, 4], [5, 6, 7, 8]]