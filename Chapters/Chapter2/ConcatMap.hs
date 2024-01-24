module Chapter2.ConcatMap where

concatMap' :: (a -> b) -> [[a]] -> [b]
concatMap' f = foldl (\acc xs -> foldr (:) (map f xs) acc) []
