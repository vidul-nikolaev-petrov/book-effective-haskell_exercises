module Chapter9.FlipTheScript where

import Prelude hiding ((<*>), fmap, pure)

-- Functor is defined in terms of Applicative
class Applicative' f =>
      Functor' f
    where
    fmap :: (a -> b) -> f a -> f b
    fmap f e = pure f <*> e

-- Applicative is defined in terms of Monad
class Monad m =>
      Applicative' m
    where
    pure :: a -> m a
    pure = return
    -- Essentially the same as:
    -- fs <*> xs = [f x | f <- fs, x <- xs]
    (<*>) :: m (a -> b) -> m a -> m b
    mf <*> ma = mf >>= \f -> ma >>= \a -> return (f a)

-- Functor' Isntance for List
instance Functor' []

-- Applicative' Isntance for List
instance Applicative' []

main :: IO ()
main = do
    print $ fmap (+ 1) [1 .. 3]
    print $ [(* 1), (* 2)] <*> [1 .. 3]
