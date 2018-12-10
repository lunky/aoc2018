 module Day9
     ( 
         day9
       , day9b
       , turn
     ) where

import Data.List (findIndex, maximumBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

input = "9 players; last marble is worth 25 points"

day9 input = snd $ maximumBy (\(_,x) (_,y) -> compare x y ) 
                 $ Map.toList
                 $ Map.fromListWith (+)
                 $ map (\((_,_,x,_),index) -> (index,x))
                 $ (\(players, marbles) -> zip  (take (marbles+1) $ turnForever ) (cycle [1..players]) )
                 $ parseInput input
                 
day9b input = 0

parseInput :: String -> (Int, Int)
parseInput input = (\y -> (read (y!!0), read (y!!6))) $ words input

deleteByIndex :: Int -> [a] -> [a] 
deleteByIndex _ [] = []
deleteByIndex n xs = take n xs ++ (drop (n+1)) xs

turnForever = iterate turn (0,0,0,[0])

turn :: (Int, Int, Int,[Int]) -> (Int, Int,Int,[Int])
turn (0,0,0,[0]) = (1, 1, 1, [0,1])
turn (1,1,1, [0,1]) = (2, 2, 2, [0,2,1])
turn (index, position, score, list) 
 | (index+1)`mod` 23 == 0 = (index+1, list!!(findIdx'+1), index+1 + list!!findIdx', deleteByIndex findIdx' list )
 | otherwise = (index+1, index+1, 0, newList)
  where newList = (take findIdx list) ++ index+1:(drop findIdx list)
        findIdx = case (findIndex (==position) list) of 
                    Nothing -> error "shouldn't be here"
                    Just i -> if (i+2>(length list)) then 
                            i - (length list) + 2
                         else 
                            i + 2
        findIdx' = case (findIndex (==index) list) of 
                    Nothing -> error "shouldn't be here"
                    Just i -> if (i-7<0) then 
                            i - 7 + ( length list )
                         else 
                            i - 7