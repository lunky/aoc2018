module Day8Spec where

import Day8
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
            
    describe "Day8" $ 
        it "should run a test case" $ do
            let input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
            let expected = 138
            day8 input `shouldBe` expected
    describe "Day8b" $ 
        it "should run a test case" $ do
            let input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
            let expected = 66
            day8b input `shouldBe` expected
