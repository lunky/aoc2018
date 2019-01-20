module Day14Spec where

import Day14
import Test.Hspec

import Data.Sequence (Seq)
import Data.Sequence as Seq

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should runa noop test" $ 
            1 `shouldBe` 1
    describe "day14" $ do
        it "should match the simple test case" $ do
            let input =  9
            let expected = "5158916779"
            day14 input `shouldBe` expected
        it "should match the simple test case" $ do
            let input =  2018
            let expected = "5941429882"
            day14 input `shouldBe` expected
    describe "day14b" $ do
        it "should match the simple test case" $ do
            let input =  "51589"
            let expected = 9
            day14b input `shouldBe` expected
        it "should match the simple test case" $ do
            let input = "01245" 
            let expected = 5
            day14b input `shouldBe` expected
        it "should match the simple test case" $ do
            let input = "92510" 
            let expected = 18
            day14b input `shouldBe` expected
        it "should match the simple test case" $ do
            let input = "59414" 
            let expected = 2018
            day14b input `shouldBe` expected
    describe "nextBoard" $ do
        it "should match the first test case" $ do
            let input = ScoreBoard 0 1 (Seq.fromList[3,7])
            let expected = ScoreBoard 0 1 (Seq.fromList[3,7,1,0])
            nextBoard input `shouldBe` expected
        it "should match the second test case" $ do
            let input = ScoreBoard 0 1 (Seq.fromList[3,7,1,0])
            let expected = ScoreBoard 4 3 (Seq.fromList[3,7,1,0,1,0])
            nextBoard input `shouldBe` expected
        it "should match the third test case" $ do
            let input = ScoreBoard 4 3 (Seq.fromList[3,7,1,0,1,0])
            let expected = ScoreBoard 6 4 (Seq.fromList[3,7,1,0,1,0,1])
            nextBoard input `shouldBe` expected
        it "should match the fourth test case" $ do
            let input = ScoreBoard 6 4 (Seq.fromList[3,7,1,0,1,0,1])
            let expected = ScoreBoard 0 6 (Seq.fromList[3,7,1,0,1,0,1,2])
            nextBoard input `shouldBe` expected
        it "should match the fifth test case" $ do
            let input = ScoreBoard 0 6 (Seq.fromList[3,7,1,0,1,0,1,2])
            let expected = ScoreBoard 4 8 (Seq.fromList[3,7,1,0,1,0,1,2,4])
            nextBoard input `shouldBe` expected
