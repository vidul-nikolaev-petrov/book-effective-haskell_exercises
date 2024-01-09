module Chapter1.Factoriel where

factoriel n
  | n == 1 = 1
  | otherwise = n * factoriel (n - 1)

main = print $ factoriel 5
