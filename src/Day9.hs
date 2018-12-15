module Day9
( 
    day9
  , day9b
  , turn
) where

import Data.List (findIndex, maximumBy, splitAt)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Sequence (Seq, (<|) )
import qualified Data.Sequence as Seq

input = "9 players; last marble is worth 25 points"

day9 :: String -> Int
day9 input =  uncurry day9common $ parseInput input
            
day9common players marbles =  snd 
            $ maximumBy (\(_,x) (_,y) -> compare x y ) 
            $ Map.toList
            $ Map.fromListWith (+)
            $ filter (\(_,y) -> y/=0)
            $ zip  (cycle [1..players]) 
            $ take (marbles+1) 
            $ map (\(_,_,_,y,_) -> y) turnForever 
            
day9b :: String -> Int
day9b input =  (\(players,marbles) -> day9common players (marbles * 100)) 
            $ parseInput input

parseInput :: String -> (Int, Int)
parseInput input = (\y -> (read (head y), read (y!!6))) $ words input

turnForever :: [(Int, Int, Int, Int, Seq Int)]
turnForever = iterate turn (0,0,0,0, Seq.fromList [0])

turn :: (Int, Int, Int,Int,Seq Int) -> (Int, Int,Int,Int,Seq Int)
turn (index, loc, position, score, list) 
   | (index+1)`mod` 23 == 0 = (index+1, circularSubtractSeven loc list, list `Seq.index` (findIdx'+1), index+1 + (list `Seq.index` findIdx'), newList')
   | otherwise = (index+1, circularAddTwo loc list, index+1, 0, newList)
   where newList = Seq.insertAt findIdx (index+1) list 
         newList' = Seq.deleteAt findIdx' list 
         findIdx = circularAddTwo loc list
         findIdx' = circularSubtractSeven loc list

circularAddTwo :: Int -> Seq a -> Int
circularAddTwo i list =  if i+2 > len then i - len+2 else i+2
   where len = Seq.length list

circularSubtractSeven :: Int -> Seq a -> Int
circularSubtractSeven i list =  if i-7 < 0 then i -7 + len else i-7
   where len = Seq.length list
