{-# LANGUAGE InstanceSigs #-}

module Chapter6.Nullable where

import Prelude hiding (null)

class Nullable a where
    isNull :: a -> Bool
    null :: a

instance Nullable (Maybe a) where
    isNull :: Maybe a -> Bool
    isNull Nothing = True
    isNull _ = False

    null :: Maybe a
    null = Nothing

instance (Nullable a, Nullable b) => Nullable (a, b) where
    isNull :: (Nullable a, Nullable b) => (a, b) -> Bool
    isNull (a, b) = isNull a && isNull b

    null :: (a, b)
    null = (null, null)

instance Nullable [a] where
    isNull :: [a] -> Bool
    isNull [] = True
    isNull _ = False

    null :: [a]
    null = []

nullMaybe :: Maybe String
nullMaybe = null

nullTuple :: (String, String)
nullTuple = null

nullList :: [Int]
nullList = null

main :: IO ()
main = do
    print $ isNull (Just 1)
    print $ isNull Nothing
    print $ isNull ("a", [])
    print $ isNull (Nothing, [])
    print $ isNull [1 ..]
    print $ isNull []
    print nullMaybe
    print nullList
    print nullTuple