{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter9.SortedListFunctor
    ( SortedList
    , insertSorted
    ) where

import Prelude hiding (fmap)

data SortedList a
    = Empty
    | Cons a (SortedList a)
    deriving stock (Eq, Show)

insertSorted :: (Ord a) => a -> SortedList a -> SortedList a
insertSorted a Empty = Cons a Empty
insertSorted a (Cons b bs)
    | a >= b = Cons b (insertSorted a bs)
    | otherwise = Cons a (Cons b bs)

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' SortedList where
    fmap' :: (a -> b) -> SortedList a -> SortedList b
    fmap' f Empty = Empty
    fmap' f (Cons a as) = Cons (f a) (fmap' f as)

main :: IO ()
main = do
    -- The following works.
    print $ fmap' (+ 2) (Cons 3 Empty)
    -- The following won't work because `insertSorted`
    -- expects the type `SortedList` as its second argument
    -- but it is applied to the type variable.
    -- print $ fmap' (insertSorted 2) (Cons 3 Empty)
