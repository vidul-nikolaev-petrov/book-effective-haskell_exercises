module Chapter7.NestedIOActions where

import Control.Monad (join)

nestedIOActions :: a -> IO (IO a)
nestedIOActions a = return $ return a

printNestedIOActions :: IO (IO String) -> IO ()
printNestedIOActions io = do
    outer <- io
    inner <- outer
    putStrLn inner

fromNestedIOActions :: IO (IO a) -> IO a
fromNestedIOActions io = do join io

printString :: IO String -> IO ()
printString io = do
    string <- io
    putStrLn string

main :: IO ()
main = do
    let s = "abc"

    -- A function of type IO (IO String)
    -- which prints the string:
    printNestedIOActions $ nestedIOActions s

    -- What happens if you try to use (>>=)?
    nestedIOActions s >>= printString

    -- A function that has the type signature: IO (IO a) -> IO a
    (fromNestedIOActions . nestedIOActions $ s) >>= putStrLn
