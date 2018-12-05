
module Day5Spec where
import Day5
import Test.Hspec

spec :: Spec
spec =  do
    describe "test harness" $
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day5" $
        it "should run test case" $ do
            let input = ""
            let expected = 0
            day5 input `shouldBe` expected