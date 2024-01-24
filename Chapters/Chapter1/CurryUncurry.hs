module Chapter1.CurryUncurry where

curry' f = \a b -> f (a, b)

curried = curry' (\t -> fst t * snd t)

uncurry' f = \t -> f (fst t) (snd t)

uncurried = uncurry' (*)

-- main = do
--   print $ curried 2 3
--   print $ uncurried (2, 3)
