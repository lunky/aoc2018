module Day1Spec where

import Day1
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $ do
            1 `shouldBe` 1
    describe "day1" $ do
        it "should start at zero" $ do
            let input = []
            let expected = 0
            day1 input `shouldBe` expected
        it "should increment from zero" $ do
            let input = "+1"
            let expected = 1
            day1 input `shouldBe` expected
    describe "day1b" $ do
        -- it "should start at zero" $ do
        --     let input = []
        --     let expected = 0
        --     day1b input `shouldBe` expected
        it "should reach 0 twice first" $ do
            let input = "+1\n-1"
            let expected = Just 0
            day1b input `shouldBe` expected
        it "should reach 10 twice first" $ do
            let input = "+3\n+3\n+4\n-2\n-4"
            let expected = Just 10
            day1b input `shouldBe` expected
        it "should reach 5 twice first" $ do
            let input = "-6\n+3\n+8\n+5\n-6"
            let expected = Just 5
            day1b input `shouldBe` expected
        it "should reach 14 twice first" $ do
            let input = "+7\n+7\n-2\n-7\n-4"
            let expected = Just 14
            day1b input `shouldBe` expected