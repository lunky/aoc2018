 module Day7
     ( 
         day7
       , day7b
       , next
       , preReqs
     ) where

import Data.List (sort, nub, findIndex)
import Data.Maybe (fromJust)

--input =  "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.\n"

day7 input = walk [] start steps
  where steps = parseInput input
        start = filter (\y -> not$any (\x -> y==snd x) steps ) set 
        set = nub $ sort $ concatMap(\(a,b) -> [a,b]) steps
  
next ch finished steps' = map snd $ filter (\(a,b) -> all (\y -> elem y finished) $ preReqs b steps') $ filter (\(a,b) -> a == ch) steps'
  where set = nub $ sort $ concatMap(\(a,b) -> [a,b]) steps'

walk :: Ord a => [a] -> [a] -> [(a, a)] -> [a]
walk finished [] _ = finished
walk finished (x:xs) steps = walk (finished ++[x]) (sort $ xs ++ (next x (finished++[x]) steps) ) (steps)

day7b input =  0

walk' finished [] _ = finished
walk' finished (x:xs) steps = walk' (finished ++[x]) (sort $ xs ++ (next x (finished++[x]) steps) ) (steps)

parseInput input = map (\y -> rescue $ words y) $ lines input
  where rescue x = (head $ x!!1, head $ x!!7) 

preReqs x []= []
preReqs x steps = nub $ sort $ concatMap (\y -> (fst y:preReqs (fst y) steps)) $ filter (\(a,b) -> b==x) steps

time ch = 61 + (fromJust $ findIndex (\y -> y==ch)  ['A'..'Z'])