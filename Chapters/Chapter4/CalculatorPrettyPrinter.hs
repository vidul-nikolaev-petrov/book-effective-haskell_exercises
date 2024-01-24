module CalculatorPrettyPrinter where

import EvalDivisionbyZero (Expr (..), safeEval)

prettyPrint' :: Expr -> String
prettyPrint' expr =
    case expr of
        Lit n -> show n
        Add e1 e2 -> prettyPrint'' " + " e1 e2
        Sub e1 e2 -> prettyPrint'' " - " e1 e2
        Mul e1 e2 -> prettyPrint'' " * " e1 e2
        Div e1 e2 -> prettyPrint'' " / " e1 e2
  where
    prettyPrint'' :: String -> Expr -> Expr -> String
    prettyPrint'' op e1 e2 =
        let result =
                ( prettyPrint' e1
                    <> op
                    <> prettyPrint' e2
                )
         in brackets result

    brackets :: String -> String
    brackets s = "( " <> s <> " )"

prettyPrint :: Expr -> String
prettyPrint expr =
    let
        evalString = either . safeEval $ expr
        prettyString = prettyPrint' expr
        -- my naive solution, see the proper one here:
        -- https://shorturl.at/dlCUV
        prettyString' = tail . init $ prettyString
     in
        prettyString' <> " = " <> evalString
  where
    either :: Either String Int -> String
    either (Left x) = x
    either (Right x) = show x

main = do
    putStrLn $ prettyPrint $ Lit 5 `Add` Lit 10
    putStrLn $ prettyPrint $ Lit 5 `Add` (Lit 10 `Div` Lit 2)
    putStrLn $ prettyPrint $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
    putStrLn $ prettyPrint $ Lit 0 `Div` Lit 10
