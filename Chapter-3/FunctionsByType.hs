module Chapter3.FunctionsByType where

-- Data.Tuple.swap :: (a,b) -> (b,a)
-- my expectation:
swap t = (snd t, fst t)

-- concat :: [[a]] -> [a]
-- my expectation:
concat' = foldr (flip (foldr (:))) []

-- id :: a -> a
-- my expectation:
id' a = a

main = do
    print $ swap (1, 2)
    print $ concat' [[1], [11, 22], [111, 222, 333]]
    print $ id' "ID function"
