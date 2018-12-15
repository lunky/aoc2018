module Day9Spec where

    import Day9
    import Test.Hspec
    import qualified Data.Sequence as Seq
    import Data.Sequence (Seq)
    
    spec :: Spec
    spec = do
        describe "Day9 turn" $ do
            it "should give the next board state" $ do
                let input = (2,1,2,0,Seq.fromList [0,2,1])
                let expected = (3,3,3,0,Seq.fromList [0,2,1,3])
                turn input `shouldBe` expected
            it "should give the next board state" $ do
                let input = (3,3,3,0,Seq.fromList [0,2,1,3])
                let expected = (4,1,4,0,Seq.fromList [0,4,2,1,3])
                turn input `shouldBe` expected
            it "should give the next board state" $ do
                let input = (0,0,0,0,Seq.fromList [0])
                let expected = (19,7,19, 0,Seq.fromList [0,16,8,17,4,18,9,19,2,10,5,11,1,12,6,13,3,14,7,15])
                (iterate turn input !! 19) `shouldBe` expected
            it "different behaviour on multiples of 23" $ do
                let input = (0,0,0,0,Seq.fromList [0])
                let expected = (23, 6, 19, 32, Seq.fromList [0,16,8,17,4,18,19,2,20,10,21,5,22,11,1,12,6,13,3,14,7,15])
                (iterate turn input !! 23) `shouldBe` expected
        describe "Day9" $ do
            it "should run a test case" $ do
                -- pendingWith "not ready"
                let input = "10 players; last marble is worth 25 points"
                let expected = 32
                day9 input `shouldBe` expected
            it "should run a test case" $ do
                -- pendingWith "not ready"
                let input = "10 players; last marble is worth 1618 points"
                let expected = 8317
                day9 input `shouldBe` expected
            it "should run a test case" $ do
                -- pendingWith "not ready"
                let input = "13 players; last marble is worth 7999 points"
                let expected = 146373
                day9 input `shouldBe` expected
                
    