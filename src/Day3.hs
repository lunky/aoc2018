{-# LANGUAGE TupleSections #-}
module Day3
    ( 
        day3
       ,day3b
    ) where

import Data.List (group, groupBy, sortBy, delete)

sample = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"

day3 :: String -> Int
day3 input = length $ getOverlap input 

getOverlap :: String -> [[((Int,Int), String)]]
getOverlap input = filter (\y -> length y >1) $ groupByPoint  plotPoints
    where plotPoints = concatMap (points . parseClaim)  $ lines input
          groupByPoint input = groupBy(\(x,_) (y,_) -> x == y) $ sortBy (\(x,_) (y,_) -> compare x y) input
    

parseInput :: String -> [String]
parseInput = lines 

parseClaim :: String -> (String,(Int,Int),(Int,Int))
parseClaim input = (claim,coords,gridSize)
    where claimData = words input
          claim = head claimData
          coords = getCoords $ claimData !! 2
          gridSize = getGridSize $ last claimData

getCoords :: String -> (Int,Int)
getCoords input = read ("(" ++ filter (/=':') input ++ ")")

points :: (String, (Int,Int), (Int,Int)) -> [((Int,Int), String)]
points (name, (x,y), (width, height)) = map  (,name) [(a,b)  | a <- [x..x+width-1], b <- [y..y+height-1] ] 

getGridSize :: String -> (Int,Int)
getGridSize input = read ("(" ++ map (\y -> if y =='x' then ',' else y) input ++ ")")

day3b :: String -> Int 
day3b input =  read $ filter (/='#') $ head $ foldr  delete claims overlaped
    where claims = map ((\ (a, _, _) -> a) . parseClaim) (lines input)
          overlaped = map snd $  concat $ getOverlap input 
