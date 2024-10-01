{-# LANGUAGE InstanceSigs #-}

module Chapter9.ExtendedFunctorFamily where

import Data.Bifunctor (Bifunctor, bimap)

data Either' a b
    = Left' a
    | Right' b
    deriving (Show)

instance Bifunctor Either' where
    bimap :: (a -> b) -> (c -> d) -> Either' a c -> Either' b d
    bimap f _ (Left' a) = Left' (f a)
    bimap _ f (Right' a) = Right' (f a)

main :: IO ()
main = do
    print $ bimap (const "Failure") (const "Success") (Right' 1)
    print $ bimap (const "Failure") (const "Success") (Left' 0)
    -- print "Extended Functor Family"
