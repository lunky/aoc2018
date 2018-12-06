 module Day6
     ( 
         day6
       , day6b
       , collectRemovingCollision
     ) where
     
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"

day6 input =  maximum 
                $ map length 
                $ groupBy (\a b -> snd a == snd b) 
                $ sortBy (\a b -> snd a `compare` snd b) 
                $ map (\y -> (fst y, fst $ head $ snd y)) 
                $ filter (\y -> snd y /=[] ) 
                $ map (\y -> (y, closest' y parsedInput)) grid
    where grid = getGrid parsedInput
          parsedInput = parseInput input
          set = map(\y -> (y,closest' y parsedInput)) grid
          

day6b input = 0

getGrid input = grid min max
    where min = minimumBy coordCompare input
          max = maximumBy coordCompare input
          grid (a,b) (c,d) = [ (x,y) | x <- [(a)..(c)], y <- [(b)..(d)]]  
          
coordCompare (a, b) (c, d) = compare (a+b) (c+d) 

distance (a,b) (c,d) = abs (c-a) + abs (d-b)

closest x xs  =  head $ sortBy (\(_,y) (_,z)  -> compare y z ) $ map (\y -> (y, distance x y) ) xs

parseInput input = map ( ( \(x : [y]) -> (x, y)) 
                        . (map (\ x -> read x :: Int) 
                        . words 
                        . filter (/= ',')) )  
                        $ lines input
                        
closest' x xs = (\y -> if (length y>1) then [] else (take 1 y) )
                 $ head  
                 $ groupBy (\a b -> snd a == snd b)  
                 $ sortBy (\(_,y) (_,z)  -> compare y z ) 
                 $ map (\y -> (y, distance x y) ) xs

collectRemovingCollision :: Ord k => k -> a -> M.Map k a -> M.Map k a
collectRemovingCollision key el set 
    | M.member key set == True = M.delete key set
    | otherwise = M.insert key el set
    
