{-# LANGUAGE InstanceSigs #-}

module Chapter6.Peano where

data Peano = Z | S Peano

instance Show Peano where
    show :: Peano -> String
    show Z = "Z"
    show (S a) = "(S " <> show a <> ")"

toPeano :: (Eq t, Num t) => t -> Peano
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: (Num a) => Peano -> a
fromPeano Z = 0
fromPeano (S a) = 1 + fromPeano a

printPeano :: IO ()
printPeano = do
    print $ toPeano 9
    print $ fromPeano . toPeano $ 128
