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

newtype BasicNullable = BasicNullable
    {getString :: Maybe String}
    deriving (Nullable) via (Maybe String)

newtype TransitiveNullable = TransitiveNullable
    {getNonEmptyString :: Maybe String}

instance Nullable TransitiveNullable where
    isNull :: TransitiveNullable -> Bool
    isNull (TransitiveNullable Nothing) = True
    isNull (TransitiveNullable (Just x)) =
        case x of
            [] -> True
            _ -> False

    null :: TransitiveNullable
    null = TransitiveNullable Nothing

main = do
    let basicNullable = BasicNullable (Just "")
    let basicNullable2 = BasicNullable (Just "abc")
    let transitiveNullable = TransitiveNullable (Just "")
    let transitiveNullable2 = TransitiveNullable (Just "abc")

    print $ isNull basicNullable
    print $ isNull basicNullable2
    print $ isNull transitiveNullable
    print $ isNull transitiveNullable2