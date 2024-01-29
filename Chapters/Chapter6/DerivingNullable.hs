{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6.DerivingNullable where

import Prelude hiding (null)
import qualified Prelude (null)

class Nullable a where
    isNull :: a -> Bool
    null :: a

instance Nullable [a] where
    isNull :: [a] -> Bool
    isNull = Prelude.null

    null :: [a]
    null = []

instance Nullable (Maybe a) where
    isNull :: Maybe a -> Bool
    isNull Nothing = True
    isNull _ = False

    null :: Maybe a
    null = Nothing

newtype MaybeEmptyList a = MaybeEmptyList (Maybe [a])

instance Nullable (MaybeEmptyList a) where
    isNull :: MaybeEmptyList a -> Bool
    isNull (MaybeEmptyList Nothing) = True
    isNull (MaybeEmptyList (Just x)) =
        case x of
            [] -> True
            _ -> False

    null :: MaybeEmptyList a
    null = MaybeEmptyList Nothing

newtype EmptyListFalseNull a = EmptyListFalseNull (Maybe a)
    deriving (Nullable) via (Maybe a)

newtype EmptyListTrueNull a = EmptyListTrueNull (MaybeEmptyList a)
    deriving (Nullable) via (MaybeEmptyList a)

main = do
    let emptyListFalseNull = EmptyListFalseNull (Just [])
    let emptyListFalseNull2 = EmptyListFalseNull (Just [1, 2, 3])
    let emptyListTrueNull = EmptyListTrueNull (MaybeEmptyList (Just []))
    let emptyListTrueNull2 = EmptyListTrueNull (MaybeEmptyList (Just [1, 2, 3]))

    print $ isNull emptyListFalseNull
    print $ isNull emptyListFalseNull2
    print $ isNull emptyListTrueNull
    print $ isNull emptyListTrueNull2