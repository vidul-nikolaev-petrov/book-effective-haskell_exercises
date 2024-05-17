module Chapter7.WordReplacement where

import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    (path : needle : replacement : _) <- getArgs
    content <- readFile path
    let words' = words content
        result = map (replace needle replacement) words'
        unwords' = unwords result
    print unwords'

replace :: String -> String -> String -> String
replace x y z
    | x == z = y
    | otherwise = z

{-

runghc Chapters/Chapter7/WordReplacement.hs \
            Chapters/Chapter7/WordReplacement.txt worry "WORRY NOT"

-}