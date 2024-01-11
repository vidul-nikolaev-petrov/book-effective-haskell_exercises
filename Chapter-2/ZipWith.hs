{-# LANGUAGE ParallelListComp #-}

module Chapter2.ZipWith where

-- pure version
zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a : as) (b : bs) = f a b : zipWith' f as bs

-- with comprehensions
zipWithC' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWithC' _ [] _ = []
zipWithC' _ _ [] = []
zipWithC' f as bs = [f a b | a <- as | b <- bs]

-- with foldl (naive approach)
zipWithF' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWithF' _ [] _ = []
zipWithF' _ _ [] = []
zipWithF' f as bs = reverse $ foldl (\acc a -> f (fst a) (snd a) : acc) [] $ zip as bs

main :: IO ()
main = do
    print $ zipWith' (*) [1 .. 3] [10, 10, 10]
    print $ zipWithC' (*) [1 .. 3] [10, 10, 10]
    print $ zipWithF' (*) [1 .. 3] [10, 10, 10]
