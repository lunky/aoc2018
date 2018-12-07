module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
            
    describe "Day7" $ 
        it "should run a test case" $ do
            let  input = ""
            let expected = 0
            day7 input `shouldBe` expected
    describe "Day7b" $ 
        it "should run a test case" $ do
            let  input = ""
            let expected = 0
            day7b input `shouldBe` expected
