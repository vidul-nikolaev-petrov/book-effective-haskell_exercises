module Main where

import qualified Chapter1.Index as C1

-- import qualified Chapter2.Index as C2

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
  "curried 2 3" # C1.curried 2 3
  "uncurried (2, 3)" # C1.uncurried (2, 3)
  "factoriel 5" # C1.factoriel 5
  "fibonacci 10" # C1.fibs 10
  printChapter 2

-- print $ concatMap' (* 10) [[1, 2], [3, 4], [5, 6, 7, 8]]
