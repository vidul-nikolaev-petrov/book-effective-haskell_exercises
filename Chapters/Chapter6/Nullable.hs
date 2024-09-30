{-# LANGUAGE InstanceSigs #-}

module Chapter6.Nullable where

import Prelude hiding (null)
import Text.Read (Lexeme(String))

class (Eq a) =>
      Nullable a
    where
    null :: a
    isNull :: a -> Bool
    isNull a = a == null

instance (Eq a) => Nullable (Maybe a) where
    null :: (Eq a) => Maybe a
    null = Nothing

instance (Nullable a, Nullable b) => Nullable (a, b) where
    null :: (a, b)
    null = (null, null)

instance (Eq a) => Nullable [a] where
    null :: [a]
    null = []

main :: IO ()
main = do
    let nothing = Nothing :: Maybe Int
    let nullList = [] :: [Int]
    let nullTuple = (Nothing, []) :: (Maybe Int, [Int])
    print $ isNull (Just 1)
    print $ isNull nothing
    print $ isNull ("", Just 1)
    print $ isNull nullTuple
    print $ isNull [1 ..]
    print $ isNull nullList
