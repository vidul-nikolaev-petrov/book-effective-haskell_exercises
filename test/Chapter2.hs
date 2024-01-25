module Chapter2 (test) where

import qualified Chapter2.Index as C2
import Test.Hspec

test :: Spec
test =
    describe "Chapter 2" $ do
        it "concatMap'" $ do
            C2.concatMap' (* 10) [[1, 2], [3, 4]] `shouldBe` [10, 20, 30, 40]
        it "reverseListL" $ do
            C2.reverseListL [1 .. 3] `shouldBe` [3, 2, 1]
        it "reverseListR" $ do
            C2.reverseListR [1 .. 3] `shouldBe` [3, 2, 1]
        it "zipWith'" $ do
            C2.zipWith' (*) [1 .. 3] [10, 10, 10] `shouldBe` [10, 20, 30]
