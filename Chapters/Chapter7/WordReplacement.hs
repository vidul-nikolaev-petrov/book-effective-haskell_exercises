module Chapter7.WordReplacement where

import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    getConfig >>= runConfig >>= putStr

getConfig :: IO [String]
getConfig = do
    (path : needle : replacement : _) <- getArgs
    return [path, needle, replacement]

runConfig :: [String] -> IO String
runConfig (path : needle : replacement : _) = do
    content <- readFile path
    let words' = words content
        result = map (replace needle replacement) words'
        unwords' = unwords result
    return unwords'

replace :: String -> String -> String -> String
replace x y z
    | x == z = y
    | otherwise = z

{-

runghc Chapters/Chapter7/WordReplacement.hs \
            Chapters/Chapter7/WordReplacement.txt worry "WORRY NOT"

-}