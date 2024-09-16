module Chapter7.NestedIOActions where

typeOfNestedIoActions :: String -> IO (IO String)
typeOfNestedIoActions = return . return
-- print the string:
-- typeOfNestedIoActions "abc" >>= (\x -> x) >>= print

fromNestedIoActions :: IO (IO a) -> IO a
fromNestedIoActions a = a >>= id
-- print the value of `a`:
-- fromNestedIoActions (return . return $ 123) >>= print

toListIoActions :: [a] -> [IO a]
toListIoActions (x : xs) = return x : toListIoActions xs

fromListIoActions :: [IO a] -> IO [a]
fromListIoActions = foldr combine (return [])
    where
        combine io acc = io
            >>= \x -> acc
            >>= \xs -> return (x : xs)



