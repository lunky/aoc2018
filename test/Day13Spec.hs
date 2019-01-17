module Day13Spec where

import Day13
import Test.Hspec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should runa noop test" $ 
            1 `shouldBe` 1
    describe "day13" $ do
        it "should match the simple test case" $ do
            let input =  "|\nv\n|\n|\n|\n^\n|"
            let expected = (0,3)
            day13 input `shouldBe` expected
        it "should match the simple test case" $ do
            let input = "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "
            let expected = (7,3)
            day13 input `shouldBe` expected
    describe "day13b" $
        it "should find the next simple test case" $ do
            let input = "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"
            let expected = (6,4)
            day13b input `shouldBe` expected
    describe "findNextLoc" $ do
        it "should give me the next location" $ do
            let input = (1,1)
            let input2 = 'v'
            let expected = (1,2)
            findNextLoc input input2 `shouldBe` expected
        it "should give me the next location" $ do
            let input =  (1,1)
            let input2 = '^'
            let expected = (1,0) 
            findNextLoc input input2 `shouldBe` expected
        it "should give me the next location" $ do
            let input =  (1,1)
            let input2 = '>'
            let expected = (2,1) 
            findNextLoc input input2 `shouldBe` expected
        it "should give me the next location" $ do
            let input =  (1,1)
            let input2 = '<'
            let expected = (0,1)
            findNextLoc input input2 `shouldBe` expected
    describe "moveCart" $ do
        it "should move right" $ do
            let input = Cart (1,0) '>' Day13.Left
            let emptyMap = Map.fromList [((1,0),'-'),((2,0),'-')]
            let expected = Cart (2,0) '>' Day13.Left
            moveCart input emptyMap `shouldBe` expected
        it "should move left" $ do
            let input = Cart (2,0) '<' Day13.Left
            let emptyMap = Map.fromList [((1,0),'-'),((2,0),'-')]
            let expected = Cart (1,0) '<' Day13.Left
            moveCart input emptyMap `shouldBe` expected
        it "should move up" $ do
            let input = Cart (1,2) '^' Day13.Left
            let emptyMap = Map.fromList [((1,2),'|'),((1,1),'|')]
            let expected = Cart (1,1) '^' Day13.Left
            moveCart input emptyMap `shouldBe` expected
        it "should move down" $ do
            let input = Cart (1,1) 'v' Day13.Left
            let emptyMap = Map.fromList [((1,2),'|'),((1,1),'|')]
            let expected = Cart (1,2) 'v' Day13.Left
            moveCart input emptyMap `shouldBe` expected
        it "should change directions from v to the >" $ do
            let input = Cart (1,1) 'v' Day13.Left
            let emptyMap = Map.fromList [((1,2),'+'),((1,1),'|')]
            let expected = Cart (1,2) '>' Day13.Straight
            moveCart input emptyMap `shouldBe` expected
        it "should not change directions from v to v" $ do
            let input = Cart (1,1) 'v' Day13.Straight
            let emptyMap = Map.fromList [((1,2),'+'),((1,1),'|')]
            let expected = Cart (1,2) 'v' Day13.Right
            moveCart input emptyMap `shouldBe` expected
        it "should change directions from v to <" $ do
            let input = Cart (1,1) 'v' Day13.Right
            let emptyMap = Map.fromList [((1,2),'+'),((1,1),'|')]
            let expected = Cart (1,2) '<' Day13.Left
            moveCart input emptyMap `shouldBe` expected
            
        it "should change directions from > to the ^" $ do
            let input = Cart (1,2) '>' Day13.Left
            let emptyMap = Map.fromList [((1,2),'-'),((2,2),'+')]
            let expected = Cart (2,2) '^' Day13.Straight
            moveCart input emptyMap `shouldBe` expected
        it "should not change directions from > to >" $ do
            let input = Cart (1,2) '>' Day13.Straight
            let emptyMap = Map.fromList [((1,2),'-'),((2,2),'+')]
            let expected = Cart (2,2) '>' Day13.Right
            moveCart input emptyMap `shouldBe` expected
        it "should change directions from > to v" $ do
            let input = Cart (1,2) '>' Day13.Right
            let emptyMap = Map.fromList [((1,2),'-'),((2,2),'+')]
            let expected = Cart (2,2) 'v' Day13.Left
            moveCart input emptyMap `shouldBe` expected
            
        it "should change directions from < to the v" $ do
            let input = Cart (2,2) '<' Day13.Left
            let emptyMap = Map.fromList [((1,2),'+'),((2,2),'-')]
            let expected = Cart (1,2) 'v' Day13.Straight
            moveCart input emptyMap `shouldBe` expected
        it "should not change directions from < to <" $ do
            let input = Cart (2,2) '<' Day13.Straight
            let emptyMap = Map.fromList [((1,2),'+'),((2,2),'-')]
            let expected = Cart (1,2) '<' Day13.Right
            moveCart input emptyMap `shouldBe` expected
        it "should change directions from < to ^" $ do
            let input = Cart (2,2) '<' Day13.Right
            let emptyMap = Map.fromList [((1,2),'+'),((2,2),'-')]
            let expected = Cart (1,2) '^' Day13.Left
            moveCart input emptyMap `shouldBe` expected
            
        it "should change directions from ^ to the <" $ do
            let input = Cart (2,2) '^' Day13.Left
            let emptyMap = Map.fromList [((2,1),'+'),((2,2),'|')]
            let expected = Cart (2,1) '<' Day13.Straight
            moveCart input emptyMap `shouldBe` expected
        it "should not change directions from ^ to ^" $ do
            let input = Cart (2,2) '^' Day13.Straight
            let emptyMap = Map.fromList [((2,1),'+'),((2,2),'|')]
            let expected = Cart (2,1) '^' Day13.Right
            moveCart input emptyMap `shouldBe` expected
        it "should change directions from ^ to >" $ do
            let input = Cart (2,2) '^' Day13.Right
            let emptyMap = Map.fromList [((2,1),'+'),((2,2),'|')]
            let expected = Cart (2,1) '>' Day13.Left
            moveCart input emptyMap `shouldBe` expected
    describe "Chads test" $ do
        it "should crash" $ do
            let input = "-<-><-"
            let expected = (0,0)
            day13b input `shouldBe` expected
        it "should crash" $ do
            let input = "-><<"
            let expected = (2,0)
            day13b input `shouldBe` expected
        it "should crash" $ do
            let input = ">--->>--"
            let expected = (1,0)
            day13b input `shouldBe` expected
{--
  v\n|--+<-\n|\n^\n
--}
            
{--
  v
  |
--+<-
  |
  ^
--}
            