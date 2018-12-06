
module Day5Spec where
import Day5
import Test.Hspec
import Data.Char

spec :: Spec
spec =  do
    describe "test harness" $
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day5" $ 
        it "should run test case" $ do
            let input = "dabAcCaCBAcCcaDA"
            let expected = 10
            day5 input `shouldBe` expected
    describe "day5b" $ 
        it "should run test case" $ do
            let input = "dabAcCaCBAcCcaDA"
            let expected = 4 
            day5b input `shouldBe` expected