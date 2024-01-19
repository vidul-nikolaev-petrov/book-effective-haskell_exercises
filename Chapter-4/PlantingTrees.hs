module Chapter4.PlantingTrees where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

sampleBinaryTree :: BinaryTree String
sampleBinaryTree =
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
                (Branch Leaf "Branch-11" Leaf)
            )
        )

-- Turn a binary tree of strings into a pretty-printed string
showStringTree :: BinaryTree String -> String
showStringTree (Branch leftBranch a rightBranch) =
    showStringTree' (Branch leftBranch a rightBranch) 0
  where
    showStringTree' :: BinaryTree String -> Int -> String
    showStringTree' Leaf _ = "Leaf"
    showStringTree' (Branch leftBranch a rightBranch) ident =
        prettyPrint a leftBranch rightBranch (succ $ succ ident)

    prettyPrint :: String -> BinaryTree String -> BinaryTree String -> Int -> String
    prettyPrint branchValue lb rb ident =
        branchValue
            <> " ->\n"
            <> repeat' ident
            <> "Left: "
            <> showStringTree' lb ident
            <> repeat' ident
            <> "Right: "
            <> showStringTree' rb ident
            <> "\n"

    repeat' :: Int -> String
    repeat' n = take n $ repeat ' '

main = do
    putStrLn $ showStringTree sampleBinaryTree
