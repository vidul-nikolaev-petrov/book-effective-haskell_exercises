module Main (main) where

import qualified Chapter1
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Effective Haskell" $ do
        Chapter1.test
