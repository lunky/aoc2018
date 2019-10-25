module Day18
     (
          day18,
          day18b,
          boardTick,
          parseInput,
          unparseInput,
          findCycle,
          findCycle',
          tick,
          Acre(..)
     ) where

import Data.List (intercalate)
import qualified Data.Set as Set

-- Open ground (.), trees (|), or a lumberyard (#)
parseChar :: Char -> Acre
parseChar '.' = OpenGround
parseChar '|' = Trees
parseChar '#' = Lumberyard

parseAcre :: Acre -> Char
parseAcre OpenGround = '.' 
parseAcre Trees = '|'
parseAcre Lumberyard = '#'

input = ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."

findAdjacent (x,y) = [
        (x-1,y-1), (x,y-1), (x+1,y-1),
        (x-1,y),            (x+1,y),
        (x-1,y+1), (x,y+1), (x+1,y+1)
    ]

found coords grid = map (`areaAt` grid) $ 
                    filter (\(x,y)-> x>=0 && y>=0 && x<size && y<size) $ 
                    findAdjacent coords
    where size = length grid

day18'' input count = iterate boardTick input !! count
day18' input count = (\y -> length (filter (=='#') y) * length (filter (=='|') y))
                    $ day18'' input count
day18 input  = day18' input 10
           
day18b input  = day18' input (cycleStart+offset) -- (454+14)
    where cycleStart = findCycle input -- 454
          cycleLength = findCycle $ day18'' input cycleStart -- 28
          offset = (1000000000-cycleStart) `mod` cycleLength -- 14

findCycle :: String -> Int
findCycle = findCycle' 0 Set.empty 

findCycle' number dict input = 
        if Set.member input dict then number
        else findCycle' (number+1) (Set.insert input dict) board
        where board = boardTick input
{--
    1,000,000,000 
    An open acre will become filled with trees if three or more adjacent acres contained trees. 
    Otherwise, nothing happens.
    An acre filled with trees will become a lumberyard if three or more adjacent acres were lumberyards. 
    Otherwise, nothing happens.
    An acre containing a lumberyard will remain a lumberyard if it was adjacent to at least one other 
    lumberyard and at least one acre containing trees. Otherwise, it becomes open.
--}
data Acre  = OpenGround | Trees | Lumberyard 
                        deriving (Eq, Show, Read)


parseInput input = map (map parseChar) $ lines input
unparseInput input = unlines $ map (map parseAcre) input
  
boardTick input = unparseInput $ group (length z) $
                  map (\y-> tick ( areaAt y z) $ found y z) $
                  coords z
          where z = parseInput input

coords input = concatMap(\(x1,data1) -> map(\(x2,data2)-> (x1,x2)) $ zip [0..] data1) $ zip [0..] input
areaAt (x,y) areas = areas !! x !! y

grid input = map (\(indexY, data1) -> (indexY,z data1)) $  z input
    where z=zip [0..]

tick OpenGround areas = if length (filter (==Trees) areas) > 2 then Trees else OpenGround
tick Trees areas = if length (filter (==Lumberyard ) areas) > 2 then Lumberyard else Trees
tick Lumberyard areas = if elem Lumberyard areas 
    && elem Trees areas  then Lumberyard else OpenGround

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
    | n > 0 = take n l : group n (drop n l)
    | otherwise = error "Negative or zero n"

{--
    λ main
    2019-10-25 15:48:01.1367312 UTC
    day18: 594712
    day18b: 203138
    2019-10-25 15:48:53.2203548 UTC
    (52.10 secs, 30,766,865,864 bytes)
    λ
--}
