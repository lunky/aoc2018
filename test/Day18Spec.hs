module Day18Spec where

import Day18
import Test.Hspec
import Data.Set as Set

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day18" $ 
        it "should have the same result as the example" $ do
            let input  = ".#.#...|#.\n" ++
                         ".....#|##|\n" ++
                         ".|..|...#.\n" ++
                         "..|#.....#\n" ++
                         "#.#|||#|#|\n" ++
                         "...#.||...\n" ++
                         ".|....|...\n" ++
                         "||...#|.#|\n" ++
                         "|.||||..|.\n" ++
                         "...#.|..|.\n"
            let expected = 1147
            day18 input `shouldBe` expected
    describe "tick" $ do
        it "should become a tree" $ do
            let input1 = OpenGround
            let input2 = [  OpenGround,   Lumberyard, OpenGround,
                            Trees,                    Trees,
                            OpenGround,   Trees,      OpenGround]
            let expected = Trees
            tick input1 input2 `shouldBe` expected
        it "should not become a tree" $ do
            let input1 = OpenGround
            let input2 = [  OpenGround,   Lumberyard, OpenGround,
                            Trees,                    OpenGround,
                            OpenGround,   Trees,      OpenGround]
            let expected = OpenGround
            tick input1 input2 `shouldBe` expected
        it "should become a Lumberyard " $ do
            let input1 = Trees
            let input2 = [  OpenGround,   Lumberyard, OpenGround,
                            Trees,                    Lumberyard,
                            OpenGround,   Lumberyard, OpenGround]
            let expected = Lumberyard
            tick input1 input2 `shouldBe` expected
        it "should not stay a Lumberyard " $ do
            let input1 = Trees
            let input2 = [  OpenGround,   Lumberyard, OpenGround,
                            Trees,                    Lumberyard,
                            OpenGround,   Trees, OpenGround]
            let expected = Trees
            tick input1 input2 `shouldBe` expected
        it "should stay a Lumberyard" $ do
            let input1 = Lumberyard
            let input2 = [  OpenGround,   Lumberyard, OpenGround,
                            Trees,                    OpenGround,
                            OpenGround,   Trees,      OpenGround]
            let expected = Lumberyard
            tick input1 input2 `shouldBe` expected
        it "should not stay a Lumberyard" $ do
            let input1 = Lumberyard
            let input2 = [  OpenGround,   Lumberyard, OpenGround,
                            OpenGround,               OpenGround,
                            OpenGround,   OpenGround, OpenGround]
            let expected = OpenGround
            tick input1 input2 `shouldBe` expected
    describe "unparseInput" $ 
        it "should go 'there and back again'" $ do
            let input  = ".#.#...|#.\n" ++
                         ".....#|##|\n" ++
                         ".|..|...#.\n" ++
                         "..|#.....#\n" ++
                         "#.#|||#|#|\n" ++
                         "...#.||...\n" ++
                         ".|....|...\n" ++
                         "||...#|.#|\n" ++
                         "|.||||..|.\n" ++
                         "...#.|..|.\n"
            unparseInput (parseInput input) `shouldBe` input
    describe "boardTick" $ do
        it "should match first pattern" $ do
            let input1 = ".#.#...|#.\n" ++
                         ".....#|##|\n" ++
                         ".|..|...#.\n" ++
                         "..|#.....#\n" ++
                         "#.#|||#|#|\n" ++
                         "...#.||...\n" ++
                         ".|....|...\n" ++
                         "||...#|.#|\n" ++
                         "|.||||..|.\n" ++
                         "...#.|..|.\n"
            let expected =  ".......##.\n" ++
                            "......|###\n" ++
                            ".|..|...#.\n" ++
                            "..|#||...#\n" ++
                            "..##||.|#|\n" ++
                            "...#||||..\n" ++
                            "||...|||..\n" ++
                            "|||||.||.|\n" ++
                            "||||||||||\n" ++
                            "....||..|.\n" 
            boardTick input1 `shouldBe` expected
        it "should match second pattern" $ do
            let input1 =    ".......##.\n" ++
                            "......|###\n" ++
                            ".|..|...#.\n" ++
                            "..|#||...#\n" ++
                            "..##||.|#|\n" ++
                            "...#||||..\n" ++
                            "||...|||..\n" ++
                            "|||||.||.|\n" ++
                            "||||||||||\n" ++
                            "....||..|.\n" 
            let expected=   ".......#..\n" ++
                            "......|#..\n" ++
                            ".|.|||....\n" ++
                            "..##|||..#\n" ++
                            "..###|||#|\n" ++
                            "...#|||||.\n" ++
                            "|||||||||.\n" ++
                            "||||||||||\n" ++
                            "||||||||||\n" ++
                            ".|||||||||\n"
            boardTick input1 `shouldBe` expected

        it "should match last pattern" $ do
            let input1 = ".#.#...|#.\n" ++
                         ".....#|##|\n" ++
                         ".|..|...#.\n" ++
                         "..|#.....#\n" ++
                         "#.#|||#|#|\n" ++
                         "...#.||...\n" ++
                         ".|....|...\n" ++
                         "||...#|.#|\n" ++
                         "|.||||..|.\n" ++
                         "...#.|..|.\n"
            let expected = 
                        ".||##.....\n" ++
                        "||###.....\n" ++
                        "||##......\n" ++
                        "|##.....##\n" ++
                        "|##.....##\n" ++
                        "|##....##|\n" ++
                        "||##.####|\n" ++
                        "||#####|||\n" ++
                        "||||#|||||\n" ++
                        "||||||||||\n"
            (iterate boardTick input1 !!10)
                `shouldBe` expected

    describe "findCycle" $ do
        it "should find pattern 0" $ do
            let input = ".#.#...|#.\n" ++
                        ".....#|##|\n" ++
                        ".|..|...#.\n" ++
                        "..|#.....#\n" ++
                        "#.#|||#|#|\n" ++
                        "...#.||...\n" ++
                        ".|....|...\n" ++
                        "||...#|.#|\n" ++
                        "|.||||..|.\n" ++
                        "...#.|..|.\n"
            let expected = 0
            findCycle' 0 (Set.fromList [input]) input `shouldBe` expected 

        it "should find pattern" $ do
            let input = ".#.#...|#.\n" ++
                        ".....#|##|\n" ++
                        ".|..|...#.\n" ++
                        "..|#.....#\n" ++
                        "#.#|||#|#|\n" ++
                        "...#.||...\n" ++
                        ".|....|...\n" ++
                        "||...#|.#|\n" ++
                        "|.||||..|.\n" ++
                        "...#.|..|.\n"
            let known = ".||##.....\n" ++
                        "||###.....\n" ++
                        "||##......\n" ++
                        "|##.....##\n" ++
                        "|##.....##\n" ++
                        "|##....##|\n" ++
                        "||##.####|\n" ++
                        "||#####|||\n" ++
                        "||||#|||||\n" ++
                        "||||||||||\n"
            let expected = 10
            findCycle' 0 (Set.fromList [known]) input `shouldBe` expected 







