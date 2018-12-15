 module Day7
     ( 
         day7
       , day7a
       , day7b
       , next
       , preReqs
     ) where

import Data.List (sort, nub, findIndex, union, elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (ap)
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map (delete, filter, filterWithKey, fromListWith, keys, lookupMin, map, null)
import Data.Set (Set)
import qualified Data.Set as Set (delete, empty, null, singleton, union)



input =  "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.\n"

day7 input = walk [] start steps
  where steps = parseInput input
        start = filter (\y -> not$any (\x -> y==snd x) steps ) set 
        set = nub $ sort $ concatMap(\(a,b) -> [a,b]) steps
  
next ch finished steps' = map snd 
                              $ filter (\(a,b) -> all (`elem` finished) $ preReqs b steps') 
                              $ filter (\(a,b) -> a == ch) steps'

walk :: Ord a => [a] -> [a] -> [(a, a)] -> [a]
walk finished [] _ = finished
walk finished (x:xs) steps = walk (finished ++[x]) (sort $ xs ++ next x (finished++[x]) steps ) steps



parseInput input = map ( rescue . words ) $ lines input
  where rescue x = (head $ x!!1, head $ x!!7) 
  
parseInput' input = Map.fromListWith Set.union $ concatMap (rescue . words) $ lines input
  where rescue x = [(head$ x!!1, Set.empty), (head $ x!!7, Set.singleton $ head $ x!!1)]
  

preReqs x []= []
preReqs x steps = nub $ sort $ concatMap (\y -> fst y:preReqs (fst y) steps) $ filter (\(a,b) -> b==x) steps

time ch cost = cost +  fromJust (elemIndex ch ['A'..'Z'])

sortSteps :: Ord k => Map k (Set k) -> [k]
sortSteps prereqs = case Map.lookupMin $ Map.filter Set.null prereqs of
            Just (k, _) -> k : sortSteps (Map.map (Set.delete k) $ Map.delete k prereqs)
            _ -> []
            
day7a input = sortSteps $ parseInput' input

day7b :: Int -> Int -> String -> Int
day7b cost workers input = (\y -> step y (alreadyReady y)) $ parseInput' input
  where tick t c = (t + cost + ord c - ord 'A' + 1, c)
        alreadyReady input = map (tick 0) $ take workers $ Map.keys $ Map.filter Set.null input
        step prereqs ((t, k):ready)
          | Map.null prereqs' && null ready' = t
          | otherwise = step prereqs' ready'
          where prereqs' = Map.map (Set.delete k) $ Map.delete k prereqs
                ok k a = k `notElem` map snd ready && Set.null a
                pending = map (tick t) $ Map.keys $ Map.filterWithKey ok prereqs'
                ready' = sort $ ready ++ take (workers - length ready) pending