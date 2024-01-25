-- | = curried and uncurried
module Chapter1.CurryUncurry where

curry' f = \a b -> f (a, b)

{- |
> curried 2 3 -- "6"
-}
curried = curry' (\t -> fst t * snd t)

uncurry' f = \t -> f (fst t) (snd t)

{- |
> uncurried (2, 3) -- "6"
-}
uncurried = uncurry' (*)
