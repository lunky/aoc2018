
module Day4Spec where
import Day4
import Test.Hspec

spec :: Spec
spec =  do
    describe "test harness" $
        it "should run a noop test" $ 
            1 `shouldBe` 1