module Day6Spec where

import Day6
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "Day6" $ 
        it "should run a test case" $ do
            let input = ""
            let expected = 0
            day6 input `shouldBe` expected
