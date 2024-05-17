module Main where

import qualified Chapter1.Index as C1
import qualified Chapter2.Index as C2
import qualified Chapter5.Index as C5
import qualified Chapter6.Index as C6
import qualified Chapter7.Index as C7

infixr 7 #
(#) :: (Show a) => String -> a -> IO ()
(#) label a = putStrLn $ (label <> ": " <> show a)

printChapter :: Int -> IO ()
printChapter n = putStrLn $ title <> underline
 where
  title = "\nChapter " <> padding <> "\n"
  padding =
    case n < 10 of
      True -> " " <> show n
      False -> show n
  underline = foldr (:) "" $ replicate 10 '-'

main :: IO ()
main = do
  printChapter 1
  "curried 2 3" # C1.curried 2 3
  "uncurried (2, 3)" # C1.uncurried (2, 3)
  "factoriel 5" # C1.factoriel 5
  "fibonacci 10" # C1.fibs 10

  printChapter 2
  "concatMap' (* 10) [[1, 2], [3, 4]]" # C2.concatMap' (* 10) [[1, 2], [3, 4]]
  "reverseListL [1 .. 3]" # C2.reverseListL [1 .. 3]
  "reverseListR [1 .. 3]" # C2.reverseListR [1 .. 3]
  "zipWith'  (*) [1 .. 3] [10, 10, 10]" # C2.zipWith' (*) [1 .. 3] [10, 10, 10]
  "zipWithC' (*) [1 .. 3] [10, 10, 10]" # C2.zipWithC' (*) [1 .. 3] [10, 10, 10]
  "zipWithF' (*) [1 .. 3] [10, 10, 10]" # C2.zipWithF' (*) [1 .. 3] [10, 10, 10]

  printChapter 5
  "does user 'george' exist" # C5.lookupUserBool "george"
  "does user 'jack' exist" # C5.lookupUserBool "jack"

  printChapter 7
  "command line calculator +" # "runghc Chapters/Chapter7/CommandLineCalculator.hs + 1 2 3"
  "command line calculator -" # "runghc Chapters/Chapter7/CommandLineCalculator.hs - 33 2 1"
  "command line calculator *" # "see the comments in Chapters/Chapter7/CommandLineCalculator.hs"
  "word replacement utility " # "see the comments in Chapters/Chapter7/WordReplacement.hs"
