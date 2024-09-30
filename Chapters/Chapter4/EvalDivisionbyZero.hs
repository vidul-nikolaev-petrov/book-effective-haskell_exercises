module EvalDivisionbyZero where

data Expr
    = Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr

safeEval :: Expr -> Either String Int
safeEval expr =
    case expr of
        Lit n -> Right n
        Add e1 e2 -> eval' (right (+)) e1 e2
        Sub e1 e2 -> eval' (right (-)) e1 e2
        Mul e1 e2 -> eval' (right (*)) e1 e2
        Div e1 e2 -> eval' eitherDiv e1 e2
  where
    right :: (Int -> Int -> Int) -> Int -> Int -> Either String Int
    right f a b = Right $ f a b
    eitherDiv :: Int -> Int -> Either String Int
    eitherDiv a b
        | a == 0 || b == 0 = Left "Error: division by zero"
        | otherwise = Right $ div a b
    eval' :: (Int -> Int -> Either String Int) -> Expr -> Expr -> Either String Int
    eval' operator e1 e2 =
        case safeEval e1 of
            Left err -> Left err
            Right a ->
                case safeEval e2 of
                    Left err -> Left err
                    Right b -> operator a b

main = do
    print $ safeEval $ (Lit 21 `Mul` Lit 9) `Add` ((Lit 12 `Add` Lit 4) `Div` Lit 8)
    print $ safeEval $ Lit 2 `Mul` ((Lit 4 `Sub` Lit 4) `Div` Lit 8)
    print $ safeEval $ Lit 2 `Mul` ((Lit 4 `Sub` Lit 6) `Div` Lit 0)
