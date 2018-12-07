module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
            
    describe "Day7" $ 
        it "should run a test case" $ do
            let input = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.\n"
            let expected = "CABDFE"
            day7 input `shouldBe` expected
    describe "Day7b" $ 
        it "should run a test case" $ do
            let input = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.\n"
            let expected = 15 
            day7b input `shouldBe` expected

    describe "Day 7 next" $ do
        it "should return the next char available" $ do
            let input = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
            let current = 'B'
            let finished = "CAB"
            let expected = ""
            next current finished input `shouldBe` expected
        it "should return the next char available" $ do
            let input = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
            let current = 'A'
            let finished = "CA"
            let expected = "BD"
            next current finished input `shouldBe` expected
        it "should return the next char available" $ do
            let input = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
            let current = 'C'
            let finished = "C"
            let expected = "AF"
            next current finished input `shouldBe` expected
    describe "Day 7 preReqs" $ do
        it "should return the next char available" $ do
            let input = 'C'
            let expected = ""
            let steps = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
            preReqs input steps `shouldBe` expected
        it "should return the next char available" $ do
            let input = 'A'
            let expected = "C"
            let steps = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
            preReqs input steps `shouldBe` expected
        it "should return the next char available" $ do
            let input = 'B'
            let expected = "AC"
            let steps = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
            preReqs input steps `shouldBe` expected
                
                
                