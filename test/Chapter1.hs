module Chapter1 (test) where

import qualified Chapter1.Index as C1
import Test.Hspec

test :: Spec
test =
    describe "Chapter 1" $ do
        it "curried " $ do
            C1.curried 2 3 `shouldBe` 6
        it "uncurried " $ do
            C1.uncurried (2, 3) `shouldBe` 6
        it "factoriel" $ do
            C1.factoriel 5 `shouldBe` 120
        it "fibonacci" $ do
            C1.fibs 8 `shouldBe` 21
            C1.fibs 9 `shouldBe` 34
            C1.fibs 10 `shouldBe` 55
