module Chapter1.Fibonacci where

fibs n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibs (n - 1) + fibs (n - 2)
