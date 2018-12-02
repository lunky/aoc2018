module Day2Spec where

import Day2
import Test.Hspec

spec :: Spec
spec =  do
    describe "test harness" $
        it "should run a noop test" $ 
            1 `shouldBe` 1

    describe "day2" $ 
        it "should get test case " $ do
            let input = "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab"
            let expected = 12
            day2 input `shouldBe` expected

    describe "day2b" $ do
        it "should get test case " $ do
            let input = "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz"
            let expected = ["fgij"]
            day2b input `shouldBe` expected
        it "should get test case " $ do
            let input = "rbcd\nabcr"
            let expected = []
            day2b input `shouldBe` expected