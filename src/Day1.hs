module Day1
    ( 
        day1
       ,day1b
    ) where

import Data.List
import Data.Set (member)
import qualified Data.Set as S
import Data.Maybe

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs S.empty
    where   dup' [] _ = Nothing
            dup' (x:xs) s = if S.member x s 
                            then Just x
                            else dup' xs (S.insert x s)


day1 :: String -> Int
day1 input = sum $ parseInput input

parseInput :: String -> [Int]
parseInput input = map read $ map (filter (/='+')) $ lines input

day1b :: String -> Maybe Int
day1b input =  dup $ scanl (+) 0 $ cycle $  parseInput input