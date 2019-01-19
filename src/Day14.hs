 module Day14
     ( 
         day14
       , day14b
       , nextBoard
       , ScoreBoard(..)
     ) where
     
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Foldable (toList)

import Data.Char (digitToInt, intToDigit)

day14 :: Int -> String
day14 input = Prelude.take 10
            $ map intToDigit
            $ toList
            $(\(ScoreBoard a b board) -> Seq.drop input board) 
            $ head
            $ dropWhile (\(ScoreBoard _ _ y) -> Seq.length y < (10+input)) 
            $ iterate nextBoard (ScoreBoard 0 1 (Seq.fromList[3,7]))


day14b :: Int -> String 
day14b input = ""
          

data ScoreBoard = ScoreBoard { elfA :: Int, lfB :: Int, scores :: Seq Int }
    deriving (Show,Eq)

-- test = ScoreBoard 1  4  ( Seq.fromList [])

digits :: Int -> [Int]
digits = map digitToInt . show

nextBoard :: ScoreBoard -> ScoreBoard
nextBoard board@(ScoreBoard elfA elfB scores)= ScoreBoard nextA nextB nextBoard
    where nextA = circularAdd elfA (Seq.index scores elfA + 1 ) nextBoard
          nextB = circularAdd elfB (Seq.index scores elfB + 1 ) nextBoard
          nextBoard = scores Seq.><  Seq.fromList ( digits $ Seq.index scores elfA + Seq.index scores elfB)
          
                                        
circularAdd :: Int -> Int -> Seq a -> Int
circularAdd start inc list =  if start+inc >= len then (start+inc) `mod` len else start+inc
   where len = Seq.length list 