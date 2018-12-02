module Main where

import Lib
import Day1
import Day2


main :: IO ()
main = do 
    contents <- readFile "data/day1.txt"
    let answer = show $ day1 contents
    putStrLn ("day1: " ++ answer)
    let answer = show $ day1b contents
    putStrLn ("day1b: " ++ answer)


    contents <- readFile "data/day2.txt"
    let answer = show $ day2 contents
    putStrLn ("day2: " ++ answer)
    let answer = show $ day2b contents
    putStrLn ("day2b: " ++ answer)