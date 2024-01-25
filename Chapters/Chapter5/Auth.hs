{-# LANGUAGE RecordWildCards #-}

module Chapter5.Auth (
    authenticate,
    Authenticated,
    Unauthenticated,
) where

import Chapter5.User (User (..))

data Authenticated
data Unauthenticated

authenticate :: User Unauthenticated -> String -> Maybe (User Authenticated)
authenticate User{..} password
    | userPassword == password = Just User{..}
    | otherwise = Nothing
