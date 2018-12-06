 module Day5
     ( 
         day5
       ,day5b
     ) where
     
import qualified Data.Set as Set
import Data.Array
import Data.Char
import Data.Bits ((.|.))
import Text.Regex

input = "dabAcCaCBAcCcaDA"
     

day5 input = length $ react regex1 (0, input)
day5b input = minimum $ map (\y -> removeThenReact y input) ['a'..'z']

pairs = zip <*> tail

isPair :: Char -> Char -> Bool
isPair a b =  a /= b && toUpper a == b || a == toUpper b

react :: Regex -> (Int, String) -> String
react regex (len, input) 
    | len == newLen = input
    | otherwise = react regex (newLen, res)
    where res = react' regex input
          newLen = length res
          react' regex input = subRegex regex input ""    

removeThenReact :: Char -> [Char] -> Int
removeThenReact char input = length $ (\y -> react regex1 (0, y)) $ filteredChar 
    where filteredChar = filter (\y -> not$(y==char || (y == toUpper char))) input
    

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

regexString = tail $ concatMap (\a -> ['|','(', toUpper a, a,')','|', '(', a, toUpper a, ')']) ['a'..'z']
regex1 = mkRegex regexString
regexsString = tail $ map (\a -> ['(', toUpper a, a,')','|', '(', a, toUpper a, ')']) ['a'..'z']
regexs = map mkRegex $ regexsString