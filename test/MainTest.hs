module Main (main) where

import qualified Chapter1.CurryUncurry as C1_CurryUncurry
import qualified Chapter1.Factoriel as C1_Factoriel
import qualified Chapter1.Fibonacci as C1_Fibonacci
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Chapter 1" $ do
        it "curried " $ do
            C1_CurryUncurry.curried 2 3 `shouldBe` 6
        it "uncurried " $ do
            C1_CurryUncurry.uncurried (2, 3) `shouldBe` 6
        it "factoriel" $ do
            C1_Factoriel.factoriel 5 `shouldBe` 120
        it "fibonacci" $ do
            C1_Fibonacci.fibs 8 `shouldBe` 21
            C1_Fibonacci.fibs 9 `shouldBe` 34
            C1_Fibonacci.fibs 10 `shouldBe` 55
