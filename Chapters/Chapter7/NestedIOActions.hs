module Chapter7.NestedIOActions where

import Control.Monad (join)

nestedIOActions :: a -> IO (IO a)
nestedIOActions = return . return

printNestedIOActions :: IO (IO String) -> IO ()
printNestedIOActions io = do
    outer <- io
    inner <- outer
    putStrLn inner

fromNestedIOActions :: IO (IO a) -> IO a
fromNestedIOActions io = do join io

toListIOActions :: [a] -> [IO a]
toListIOActions (x : xs) = return x : toListIOActions xs

fromListIOActions :: [IO a] -> IO [a]
fromListIOActions = sequence

printString :: IO String -> IO ()
printString io = do
    string <- io
    putStrLn string

-- A function of type IO (IO String)
-- which prints the string:
-- printNestedIOActions $ nestedIOActions s

-- What happens if you try to use (>>=)?
-- nestedIOActions s >>= printString

-- A function that has the type signature: IO (IO a) -> IO a
-- (fromNestedIOActions . nestedIOActions $ s) >>= putStrLn

-- putStrLn $ toListIOActions . fromListIOActions $ [1 .. 3]
