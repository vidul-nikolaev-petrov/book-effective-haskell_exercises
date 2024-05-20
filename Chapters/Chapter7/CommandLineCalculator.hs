module Chapter7.CommandLineCalculator where

import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    putStr "Result: "
    calculateArgs >>= print

sumArgs :: IO ()
sumArgs = do
    args <- getArgs
    print . sum $ mapMaybe parseArg args

calculateArgs :: IO Int
calculateArgs = do
    (command : args) <- getArgs
    let args' = mapMaybe parseArg args
        op = case command of
            "+" -> sum
            "*" -> product
            "-" -> minus
            _ -> const 0
     in return $ op args'

minus :: (Num a) => [a] -> a
minus [x] = x
minus [x, y] = x - y
minus (x : y : xs) = foldl (-) (x - y) xs

parseArg :: String -> Maybe Int
parseArg = readMaybe

{-

runghc Chapters/Chapter7/CommandLineCalculator.hs + 1 2 3
runghc Chapters/Chapter7/CommandLineCalculator.hs - 33 2 1
runghc Chapters/Chapter7/CommandLineCalculator.hs \* 11 22 33

-}