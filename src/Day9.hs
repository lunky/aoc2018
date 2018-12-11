 module Day9
     ( 
         day9
       , day9b
       , turn
     ) where

import Data.List (findIndex, maximumBy, splitAt)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

input = "9 players; last marble is worth 25 points"

day9 input =  uncurry day9common $ parseInput input
                 
day9common players marbles =  snd 
                 $ maximumBy (\(_,x) (_,y) -> compare x y ) 
                 $ Map.toList
                 $ Map.fromListWith (+)
                 $ filter (\(_,y) -> y/=0)
                 $ zip  (cycle [1..players]) 
                 $ take (marbles+1) 
                 $ map (\(_,_,y,_) -> y) 
                 $ turnForever 
                 
day9b input =  (\(players,marbles) -> day9common players (marbles * 100)) 
                 $ parseInput input

parseInput :: String -> (Int, Int)
parseInput input = (\y -> (read (y!!0), read (y!!6))) $ words input

deleteByIndex :: Int -> [a] -> [a] 
deleteByIndex _ [] = []
deleteByIndex n xs = take n xs ++ (drop (n+1)) xs

turnForever = iterate turn (0,0,0,[0])

turnForever :: [(Int, Int, Int, [Int])]
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs
                  
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []                  
                  
turn :: (Int, Int, Int,[Int]) -> (Int, Int,Int,[Int])
turn (index, position, score, list) 
 | (index+1)`mod` 23 == 0 = (index+1, list!!(findIdx'+1), index+1 + list!!findIdx', deleteByIndex findIdx' list )
 | otherwise = (index+1, index+1, 0, newList)
  where newList = insertAt findIdx (index+1) list 
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