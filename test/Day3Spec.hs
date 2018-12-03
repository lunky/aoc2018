module Day3Spec where

import Day3
import Test.Hspec

spec :: Spec
spec =  do
    describe "test harness" $
        it "should run a noop test" $ 
            1 `shouldBe` 1

    describe "day3" $ 
        it "should get test case " $ do
            let input = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
            let expected = 4
            day3 input `shouldBe` expected
    describe "dayb3" $ 
        it "should get test case " $ do
            let input = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
            let expected = 3
            day3b input `shouldBe` expected