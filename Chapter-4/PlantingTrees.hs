module Chapter4.PlantingTrees where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

stringBinaryTree :: BinaryTree String
stringBinaryTree =
    Branch
        ( Branch
            ( Branch
                (Branch Leaf "Branch-8" Leaf)
                "Branch-4"
                (Branch Leaf "Branch-9" Leaf)
            )
            "Branch-2"
            ( Branch
                (Branch Leaf "Branch-10" Leaf)
                "Branch-5"
                (Branch Leaf "Branch-11" Leaf)
            )
        )
        "Branch-1"
        ( Branch
            (Branch Leaf "Branch-6" Leaf)
            "Branch-3"
            ( Branch
                (Branch Leaf "Branch-10" Leaf)
                "Branch-7"
                ( Branch
                    (Branch Leaf "Branch-12" Leaf)
                    "Branch-11"
                    (Branch Leaf "Branch-13" Leaf)
                )
            )
        )

intBinaryTree :: BinaryTree Int
intBinaryTree =
    Branch
        ( Branch
            ( Branch
                Leaf
                2
                Leaf
            )
            3
            ( Branch
                Leaf
                4
                Leaf
            )
        )
        5
        ( Branch
            Leaf
            7
            ( Branch
                Leaf
                8
                Leaf
            )
        )

-- Turn a binary tree of strings into a pretty-printed string
showStringTree :: BinaryTree String -> String
showStringTree (Branch left a right) =
    showStringTree' (Branch left a right) 0
  where
    showStringTree' :: BinaryTree String -> Int -> String
    showStringTree' Leaf _ = "Leaf"
    showStringTree' (Branch left a right) ident =
        prettyPrint a left right (succ $ succ ident)

    prettyPrint :: String -> BinaryTree String -> BinaryTree String -> Int -> String
    prettyPrint branchValue lb rb ident =
        branchValue
            <> " ->\n"
            <> repeat' ident
            <> "Left: "
            <> showStringTree' lb ident
            <> case rb of
                Leaf -> repeat' (ident - ident) <> ", "
                _ -> repeat' ident
            <> "Right: "
            <> showStringTree' rb ident
            <> case rb of
                Leaf -> "\n"
                _ -> ""

    repeat' :: Int -> String
    repeat' = flip replicate ' '

-- Add a new integer into a binary tree of integers
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree Leaf n = Branch Leaf n Leaf
addElementToIntTree t@(Branch left a right) n
    | n < a = Branch (addElementToIntTree left n) a right
    | n > a = Branch left a (addElementToIntTree right n)
    | otherwise = t

-- Check to see if an int value exists in a binary tree of ints
doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch left a right) n
    | n > a = doesIntExist right n
    | n < a = doesIntExist left n
    | otherwise = True

toStringTree :: (Show a) => BinaryTree a -> BinaryTree String
toStringTree Leaf = Leaf
toStringTree (Branch left a right) =
    Branch (toStringTree left) (show a) (toStringTree right)

main = do
    putStrLn $ showStringTree stringBinaryTree
    putStrLn . showStringTree . toStringTree $ addElementToIntTree intBinaryTree 6
    print $ doesIntExist intBinaryTree 5
    print $ doesIntExist intBinaryTree 6