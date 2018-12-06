 module Day6
     ( 
         day6
       , day6b
     ) where
     
import Data.List (sortBy, groupBy, maximumBy, minimumBy)

input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"

day6 input =  maximum 
                $ map length 
                $ groupBy (\a b -> snd a == snd b) 
                $ sortBy (\a b -> snd a `compare` snd b) 
                $ map (\y -> (fst y, fst $ head $ snd y)) 
                $ filter (\y -> snd y /=[] ) 
                $ map (\y -> (y, closest y parsedInput)) grid
    where grid = getGrid parsedInput
          parsedInput = parseInput input
          set = map(\y -> (y,closest y parsedInput)) grid
          

day6b input maxDistance = length 
                $ filter (\(x,y)-> y < maxDistance) 
                $ map (\y -> (y, sum $ distanceFrom y parsedInput) ) grid
    where grid = getGrid parsedInput
          parsedInput = parseInput input

getGrid :: (Foldable t, Ord b, Num b, Enum b) => t (b, b) -> [(b, b)]
getGrid input = grid min max
    where min = minimumBy coordCompare input
          max = maximumBy coordCompare input
          grid (a,b) (c,d) = [ (x,y) | x <- [a..c], y <- [b..d]]  
          
coordCompare :: (Ord a, Num a) => (a, a) -> (a, a) -> Ordering
coordCompare (a, b) (c, d) = compare (a+b) (c+d) 

distance :: Num a => (a, a) -> (a, a) -> a
distance (a,b) (c,d) = abs (c-a) + abs (d-b)

parseInput :: String -> [(Int, Int)]
parseInput input = map ( ( \(x : [y]) -> (x, y)) 
                        . (map (\ x -> read x :: Int) 
                        . words 
                        . filter (/= ',')) )  
                        $ lines input
                        
closest :: (Ord b, Num b) => (b, b) -> [(b, b)] -> [((b, b), b)]
closest x xs = (\y -> if length y>1 then [] else take 1 y )
                 $ head  
                 $ groupBy (\a b -> snd a == snd b)  
                 $ sortBy (\(_,y) (_,z)  -> compare y z ) 
                 $ map (\y -> (y, distance x y) ) xs



distanceFrom x = map (distance x)

