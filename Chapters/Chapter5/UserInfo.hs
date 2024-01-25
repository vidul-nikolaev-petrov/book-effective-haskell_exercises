module Chapter5.UserInfo where

import Chapter5.Auth (
    Authenticated,
    Unauthenticated,
 )
import Chapter5.User (User (..))

getUserName :: User isAuthenticated -> String
getUserName = userName

getUserScore :: User isAuthenticated -> Int
getUserScore = userInternetPoints

getUserEmailAddress :: User Authenticated -> String
getUserEmailAddress = userEmailAddress