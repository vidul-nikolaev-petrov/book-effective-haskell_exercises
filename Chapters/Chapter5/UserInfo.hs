module Chapter5.UserInfo where

import Chapter5.User (User (..))
import Chapter5.UserAuth (
    Authenticated,
    Unauthenticated,
 )

getUserName :: User isAuthenticated -> String
getUserName = userName

getUserScore :: User isAuthenticated -> Int
getUserScore = userInternetPoints

getUserEmailAddress :: User Authenticated -> String
getUserEmailAddress = userEmailAddress