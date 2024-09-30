module Chapter5.Users
    ( lookupUser
    , lookupUserBool
    ) where

import Chapter5.Auth (Authenticated, Unauthenticated)
import Chapter5.User (User(..))
import Data.List (find)

users :: [User a]
users = [george, porter]
  where
    george =
        User
            { userName = "george"
            , userInternetPoints = 1000
            , userPassword = "secret"
            , userEmailAddress = "gbird2015@example.com"
            }
    porter =
        User
            { userName = "porter"
            , userInternetPoints = 500
            , userPassword = "hunter2"
            , userEmailAddress = "woofwoof@example.com"
            }

lookupUser :: String -> Maybe (User Unauthenticated)
lookupUser name = find (\user -> userName user == name) users

lookupUserBool :: String -> Bool
lookupUserBool name =
    case lookupUser name of
        Nothing -> False
        (Just _) -> True
