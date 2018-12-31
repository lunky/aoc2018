 module Day12
     ( 
         day12
       , day12b
       , parseInput
       , day12process
     ) where
import Data.List (zip5, find)
input = "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"

day12 :: String -> Int    
day12 input = day12bt input 20
          
-- I came up with the 150 (and the 63) by analyzing the results as I increased 
-- iterations... after 150 there was a repeating cycle with an increase of 63.
-- If I was a better / more thorough developer I'd come up with a test
day12b :: String -> Int -> Int
day12b input iterations 
    | iterations < 150 =   day12bt input iterations
    | otherwise = day12bt input 150 + ((iterations-150)*63) 

day12bt :: String -> Int -> Int    
day12bt input iterations =   sum 
                $ map fst 
                $ filter (\(_,y) -> y=='#') 
                $ zip [-iterations..] 
                $ head 
                $ drop iterations
                $ iterate (day12process rules) initialState
    where rules = getRules input
          initialState = replicate iterations '.' ++ fst (parseInput input) ++ replicate iterations '.'


day12process :: Foldable t => t (String, Char) -> String -> String
day12process rules input = foldr (\(a,b,c,d,e) acc -> replaceIfMatch rules [a,b,c,d,e] :acc ) [] $ snd ( zippedState input )


replaceIfMatch :: (Foldable t, Eq a) => t (a, Char) -> a -> Char
replaceIfMatch rules ch = case find (\(pat, val) -> pat==ch) rules of
                             Just (p,v) -> v
                             Nothing    -> '.' 

parseInput :: String -> (String, [(String, Char)])
parseInput input = (\y -> (drop 15 $ head y, map (\y -> (take 5 y, y!!9) ) $ drop 2 y)) $ lines input

zippedState :: String -> (Int,[(Char,Char,Char,Char,Char)])
zippedState inputz = (\y -> (offset, zip5 y (drop 1 y) (drop 2 y ) (drop 3 y) (drop 4 y))) state
    where state = ".." ++ inputz ++ ".."
          offset = length $ filter (=='#') $ take 2 state
          
   
getRules :: String -> [(String, Char)]
getRules = snd.parseInput 
