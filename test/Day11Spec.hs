module Day11Spec where

import Day11
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day11 getPowerLevel" $ do
        it "should match given test case" $ do
            let input = 8
            let input2 = (3,5)
            let expected = 4
            getPowerLevel input input2 `shouldBe` expected
        it "should match given test case" $ do
            let input =57 
            let input2 = (122,79)
            let expected = -5
            getPowerLevel input input2 `shouldBe` expected
        it "should match given test case" $ do
            let input = 39
            let input2 = (217,196)
            let expected = 0 
            getPowerLevel input input2 `shouldBe` expected
        it "should match given test case" $ do
            let input = 71
            let input2 = (101,153)
            let expected = 4
            getPowerLevel input input2 `shouldBe` expected
    describe "day11 sumTbT" $ 
        it "should match given test case" $ do
            let input = 42
            let input2 = (21,61)
            let expected = 30
            sumTbT input input2 `shouldBe` expected
    describe "day11 sumTbT" $ 
        it "should match given test case" $ do
            let input = 18 
            let input2 =  (33,45)
            let expected = 29
            sumTbT input input2 `shouldBe` expected
            -- these work but they're slow
    -- describe "day11" $ do
    --     it "should match given test case" $ do
    --         let input = 18 
    --         let expected = (33,45)
    --         day11 input `shouldBe` expected
    --     it "should match given test case" $ do
    --         let input = 42
    --         let expected = (21,61)
    --         day11 input `shouldBe` expected
    describe "pointMaximum" $ do
        it " should give me test case 1" $ do 
            let serial = 18
            let point = (90,269)
            let expected = (16,113)
            pointMaximum serial point `shouldBe` expected
        it " should give me test case 2" $ do 
            let serial = 42
            let point = (232,251)
            let expected = (12,119)
            pointMaximum serial point `shouldBe` expected
        
        