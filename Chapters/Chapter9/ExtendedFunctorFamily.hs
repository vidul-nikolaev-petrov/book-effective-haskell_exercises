{-# LANGUAGE InstanceSigs #-}

module Chapter9.ExtendedFunctorFamily where

import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Functor.Contravariant (Contravariant)

data Either' a b
    = Left' a
    | Right' b
    deriving (Show)

instance Bifunctor Either' where
    bimap :: (a -> b) -> (c -> d) -> Either' a c -> Either' b d
    bimap f _ (Left' a) = Left' (f a)
    bimap _ f (Right' a) = Right' (f a)
    first :: (a -> b) -> Either' a c -> Either' b c
    first f = bimap f id
    second :: (b -> c) -> Either' a b -> Either' a c
    second = bimap id

main :: IO ()
main = do
    let first' = first (const "`first` means failure") (Left' 1) :: Either' String Int
    let second' = second (const "`second` means success") (Right' 0) :: Either' Int String
    print $ bimap (const "Failure") (const "Success") (Right' 0)
    print $ bimap (const "Failure") (const "Success") (Left' 1)
    print first'
    print second'
