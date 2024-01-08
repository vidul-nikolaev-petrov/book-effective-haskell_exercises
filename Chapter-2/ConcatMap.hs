import Data.Char

concatMap' :: (a -> b) -> [[a]] -> [b]
concatMap' f = foldr (\xs acc -> foldr (:) [f x | x <- xs] acc) []

main :: IO ()
main = do
    print $ concatMap' (show . (+ 10)) [[1, 2], [11, 22]]