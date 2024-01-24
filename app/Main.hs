module Main where

import qualified Chapter1.CurryUncurry as C1_CurryUncurry
import qualified Chapter1.Factoriel as C1_Factoriel
import qualified Chapter1.Fibonacci as C1_Fibonacci

(#) :: (Show a) => String -> a -> IO ()
(#) label a = print $ (label <> ": " <> show a)

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
  "curried 2 3" # C1_CurryUncurry.curried 2 3
  "uncurried (2, 3)" # C1_CurryUncurry.uncurried (2, 3)
  "factoriel 5" # C1_Factoriel.factoriel 5
  "fibonacci 10" # C1_Fibonacci.fibs 10
  putStrLn "\nChapter 2"
