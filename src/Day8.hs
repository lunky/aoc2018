 module Day8
     ( 
         day8
       , day8b
     ) where


data Tree a = Node [Tree a] [Int] deriving (Show) 

day8 input = sumData $ fst $ buildNode $ parseInput input
day8b input = sumData' $ fst $ buildNode $ parseInput input

sumData (Node children payload) = sum payload + sum (map sumData children)

sumData' (Node [] payload) = sum payload 
sumData' (Node children payload) = sum $ map (sumData'.getNode) payload
  where getNode offset
         | offset == 0 = Node [] []
         | offset > length children = Node [] []
         | otherwise = children!!(offset - 1)

buildNode (nKids:nMetadata: inputData) = 
 let (kids, rest) = buildChildren nKids [] inputData
 in (Node kids (take nMetadata rest), drop nMetadata rest)

buildChildren 0 siblings inputData = (siblings,inputData)
buildChildren nSiblings siblings inputData =
 let (child, rest) = buildNode inputData
 in buildChildren (nSiblings-1) (siblings ++ [child]) rest
    
parseInput :: String -> [Int]
parseInput input = map read $ words input

input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"