module Day15Spec where

import Day15
import Test.Hspec

spec :: Spec
spec = 
    describe "Lib" $ 
        it "should runa noop test" $ 
            1 `shouldBe` 1
    -- describe "day15" $ do
