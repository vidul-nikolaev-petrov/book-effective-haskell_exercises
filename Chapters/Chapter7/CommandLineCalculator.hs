module Chapter7.CommandLineCalculator where

import Data.Foldable (Foldable (fold))
import Data.Maybe (catMaybes)
import System.Environment (getArgs, withArgs)
import Text.Read (readMaybe)

printArgs :: IO ()
printArgs = getArgs >>= print

sumArgs :: IO ()
sumArgs = do
    args <- getArgs
    let maybeInts = map readMaybe args
    print . sum . catMaybes $ maybeInts

calculateArgs :: IO ()
calculateArgs = do
    args <- getArgs
    case args of
        [] -> return ()
        _ -> putStr "Result: "
    let maybeInts = map readMaybe $ tail args
        func = case head args of
            "+" -> sum
            "*" -> product
            "-" -> foldr (-) 0
            "/" -> foldr div 1
            _ -> const 0
    print . func . catMaybes $ maybeInts
