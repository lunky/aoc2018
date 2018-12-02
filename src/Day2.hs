module Day2
    ( 
        day2
       ,day2b
    ) where
import Data.List (sort, group, nub, intersect, tails)

sample2 = "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz"
sample3 = "abcde\nfghij\nklmno\npqrst\nfghij\naxcye\nwvxyz"

day2 :: String -> Int
day2 input = product $ map length $ group $ sort $ concatMap checkSum $ parseInput input

checkSum :: String -> [Int]
checkSum input = nub $ filter (\y -> y==2 || y==3) $ map length $ group $ sort input

day2b :: String -> [String]
day2b input = take 1 $ findBox input

findBox :: String -> [String]
findBox input = filter (\y-> length y == oneLess) $ concatMap matches (tails $ parseinput input)
        where  oneLess = length (head parsedBoxes) -1

matches :: Eq a => [[a]] -> [[a]]
matches [] =  [[]]
matches [_] =  []
matches (x : xs) =  map (sameish x) xs 
    where sameish a b = map fst $ filter (uncurry (==)) $ zip a b



parseInput = lines