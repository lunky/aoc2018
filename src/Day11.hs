 module Day11
     ( 
         day11
       , day11b
       , getPowerLevel
       , sumTbT
     ) where
     
import Data.List (maximumBy)
import Data.Map (Map)
import qualified Data.Map as Map
     
input = 8

getPowerLevel :: Int -> (Int, Int) -> Int
getPowerLevel serial (x,y) = hundredthsPlace (((rackId * y) + serial) * rackId) - 5
    where rackId = x+10
          hundredthsPlace i = (i `div` 100) `mod` 10
          
sumPowerLevels :: Int -> [(Int, Int)] -> Int
sumPowerLevels serial list = sum $ map (\y -> getPowerLevel serial y) list

sumPowerLevelsMap :: Map (Int,Int) Int -> [(Int, Int)] -> Int
sumPowerLevelsMap gridMap list = sum $ map (\y -> glookup gridMap y) list

day11 input = fst $ maximumBy (\(a,b) (c,d) -> (compare b d)) $ map (\y -> (y, sumTbT input y))  getGrid

getGrid :: [(Int,Int)]
getGrid =  [ (x,y) | x <- [1..300], y <- [1..300]] 

sumTbT serial start = sum $ map (\y -> getPowerLevel serial y) $ fst $ xByX (start,3)

day11b serial = fst $ maximumBy (\(a,b) (c,d) -> compare b d) $ map (\y -> day11bt gmap y) $ getGrid
    where gmap = getGridMap serial

day11bt gmap point = (\(x,y) -> ((point, y),x)) 
                        $ maximumBy (\(a,b) (c,d) ->(compare a c)) 
                        $ map (\(x,y)-> (sumPowerLevelsMap gmap x,y)) 
                        $ map xByX $ squaresXByX point
    
-- day11bt input = fst $ maximumBy (\(a,b) (c,d) -> (compare b d)) $ map (\y -> (y, sumTbT' input y))  getGrid

threeByThree (x,y) = [(x,y),   (x+1,y),   (x+2,y),
                      (x,y+1), (x+1,y+1), (x+2,y+1),
                      (x,y+2), (x+1,y+2), (x+2,y+2) ]
                      
xByX :: ((Int,Int),Int) -> ([(Int,Int)],Int)
xByX ((x,y),gridSize) = ([(x+a,y+b) | a<-[0..gridSize-1], b<-[0..gridSize-1]],gridSize)
                      
squaresXByX :: (Int, Int) -> [((Int,Int),Int)]
squaresXByX (x,y) = takeWhile (\((_,_),a) -> (a+x <= bound+1) && (a+y <= bound+1)) $ map (\z-> ((x,y),z)) [1..]
    where bound = 300

getGridMap :: Int -> Map (Int,Int) Int
getGridMap serial = Map.fromList $ map(\y -> (y,getPowerLevel serial y)) $ getGrid


-- sumTbT' serial start x = sum $ map (\y -> getPowerLevel serial y) $ xByX (start,x)
gridMap = getGridMap 18

glookup :: Map (Int,Int) Int -> (Int,Int) -> Int 
glookup map loc = gridMap Map.! loc