 module Day11
     ( 
         day11
       , day11b
       , getPowerLevel
       , sumTbT
       , pointMaximum 
     ) where
     
import Data.List (maximumBy)
     
input = 8

day11 input = fst $ maximumBy (\(a,b) (c,d) -> (compare b d)) $ map (\y -> (y, sumTbT input y))  getGrid

day11b serial = (\((a,b),c,d) -> init $ tail $ show [a,b,c] ) $ maximumBy (\(_,_,a) (_,_,b) -> compare a b) $ map (\y ->(\(a,b) -> (y,a,b)) $ pointMaximum serial y) $ getGrid

getPowerLevel :: Int -> (Int, Int) -> Int
getPowerLevel serial (x,y) = hundredthsPlace (((rackId * y) + serial) * rackId) - 5
    where rackId = x+10
          hundredthsPlace i = (i `div` 100) `mod` 10
          
sumPowerLevels :: Int -> [(Int, Int)] -> Int
sumPowerLevels serial list = sum $ map (\y -> getPowerLevel serial y) list

getGrid :: [(Int,Int)]
getGrid =  [ (x,y) | x <- [1..300], y <- [1..300]] 

sumTbT :: Int -> (Int, Int) -> Int
sumTbT serial start = sum $ map (\y -> getPowerLevel serial y) $ fst $ xByX (start,3)

xByX :: ((Int,Int),Int) -> ([(Int,Int)],Int)
xByX ((x,y),gridSize) = ([(x+a,y+b) | a<-[0..gridSize-1], b<-[0..gridSize-1]],gridSize)
                      
squaresXByX :: (Int, Int) -> [((Int,Int),Int)]
squaresXByX (x,y) = takeWhile (\((_,_),a) -> (a+x <= bound+1) && (a+y <= bound+1)) $ map (\z-> ((x,y),z)) [1..]
    where bound = 300

getNextSquare :: ((Int,Int),Int) -> [(Int,Int)]
getNextSquare ((x,y), 1) = [(x,y)]
getNextSquare ((x,y), size) = [(x+a,y+size-1) | a <- [0..size-1]] ++ [(x+size-1,y+a) | a <- [0..size-2]]


pointMaximum serial point = (\(_, b, _) -> b ) 
                        $ foldr (\y (a,b,c) ->(\z -> (a+1, maxBySnd b (a+1,c+z), c+z)) $ (sumPowerLevels serial $ getNextSquare y) ) (0,(0,0),0) 
                        $ reverse 
                        $ squaresXByX point
                        
maxBySnd a b = case compare (snd a) (snd b) of
                     GT -> a
                     _  -> b
