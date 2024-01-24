module Chapter1.CurryUncurry where

curry' f = \a b -> f (a, b)

curried = curry' (\t -> fst t * snd t)

uncurry' f = \t -> f (fst t) (snd t)

uncurried = uncurry' (*)
