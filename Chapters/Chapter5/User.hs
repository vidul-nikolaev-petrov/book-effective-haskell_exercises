module Chapter5.User
    ( User(..)
    ) where

data User isAuthenticated = User
    { userName :: String
    , userInternetPoints :: Int
    , userPassword :: String
    , userEmailAddress :: String
    }
