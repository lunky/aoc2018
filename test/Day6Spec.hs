module Day6Spec where

import Day6
import Test.Hspec
import qualified Data.Map as M

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "collectRemovingCollision" $ do
        it "should remove collision" $ do
            let key = ((1,1),2)
            let el = ((0,0),((1,1),2))
            let set = M.fromList [(((1,1),2),((0,0),((1,1),2)))]
            let expected = M.empty
            collectRemovingCollision key el set `shouldBe` expected
            
        it "should add non-collision" $ do
            let key = ((1,1),2)
            let el = ((0,0),((1,1),2))
            let set = M.fromList [(((1,1),3),((0,0),((1,1),3)))]
            let expected = M.fromList [(((1,1),2),((0,0),((1,1),2))), (((1,1),3),((0,0),((1,1),3))) ]
            -- wow reading these is hard
            collectRemovingCollision key el set `shouldBe` expected
        
    describe "Day6" $ 
        it "should run a test case" $ do
            let  input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"
            let expected = 17
            day6 input `shouldBe` expected
