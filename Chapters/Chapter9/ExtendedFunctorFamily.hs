{-# LANGUAGE InstanceSigs #-}

module Chapter9.ExtendedFunctorFamily where

import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Functor.Contravariant (Contravariant(contramap))

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

newtype F b a = F
    { runF :: a -> b
    }

instance Contravariant (F b) where
    contramap :: (a' -> a) -> F b a -> F b a'
    contramap f (F g) = F (g . f)

greaterThanTen :: Int -> Bool
greaterThanTen n = n > 10

greaterThanTenF :: F Bool Int
greaterThanTenF = F greaterThanTen

stringToInt :: String -> Int
stringToInt = read

lengthGreaterThanTenF :: F Bool String
lengthGreaterThanTenF = contramap stringToInt greaterThanTenF

class Profunctor f where
    dimap :: (c -> a) -> (b -> d) -> f a b -> f c d
    lmap :: (c -> a) -> f a b -> f c b
    lmap f = dimap f id
    rmap :: (b -> d) -> f a b -> f a d
    rmap = dimap id

newtype F' a b = F'
    { runF' :: a -> b
    }

instance Profunctor F' where
    dimap :: (c -> a) -> (b -> d) -> F' a b -> F' c d
    dimap f g (F' h) = F' (g . h . f)
    lmap :: (c -> a) -> F' a b -> F' c b
    lmap f (F' g) = F' (g . f)
    rmap :: (b -> d) -> F' a b -> F' a d
    rmap f (F' g) = F' (f . g)

main :: IO ()
main = do
    let first' = first (const "`first` means failure") (Left' 1) :: Either' String Int
    let second' = second (const "`second` means success") (Right' 0) :: Either' Int String
    let contramap' = runF lengthGreaterThanTenF "10"
    let contramap'' = runF lengthGreaterThanTenF "11"
    let length' = F' length :: F' String Int
    let runlmap' = runF' (lmap reverse length')
    let runrmap' = runF' (rmap show length')
    print $ bimap (const "Failure") (const "Success") (Right' 0)
    print $ bimap (const "Failure") (const "Success") (Left' 1)
    print first'
    print second'
    print $ runF lengthGreaterThanTenF "11"
    putStrLn $ "contramap: 10 > 10: " <> show contramap'
    putStrLn $ "contramap: 10 > 11: " <> show contramap''
    putStrLn
        $ "dimap - `reverse show (F' length) \"olleh\": ` "
              <> runF' (dimap reverse show length') "olleh"
    putStrLn $ "lmap reverse (F' length) \"olleh\": " <> show (runlmap' "olleh")
    putStrLn $ "rmap show (F' length) \"olleh\": " <> runrmap' "olleh"
