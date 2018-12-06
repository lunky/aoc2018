module Day6Spec where

import Day6
import Test.Hspec
import qualified Data.Map as M

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
            
    describe "Day6" $ 
        it "should run a test case" $ do
            let  input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"
            let expected = 17
            day6 input `shouldBe` expected
    describe "Day6b" $ 
        it "should run a test case" $ do
            let  input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"
            let  input2 = 32 
            let expected = 16
            day6b input input2 `shouldBe` expected
