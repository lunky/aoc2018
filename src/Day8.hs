 module Day8
     ( 
         day8
       , day8b
     ) where


data Tree a = Node [Int] [Tree a] deriving (Show) 

day8 input = sumData $ fst $ buildNode $ parseInput input
day8b input = 0

sumData (Node payload children) = sum payload + (sum $ map (\y -> sumData y) children )

buildNode (nKids:nMetadata: inputData) = 
 let (kids, rest) = buildChildren nKids [] inputData
 in (Node (take nMetadata rest) kids, (drop nMetadata rest))

buildChildren 0 siblings inputData = (siblings,inputData)
buildChildren nSiblings siblings inputData =
 let (child, rest) = buildNode (inputData)
 in buildChildren (nSiblings-1) ([child] ++ siblings) rest
    
parseInput :: String -> [Int]
parseInput input = map read $ words input

input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"