module Main where

import Lib
import Day1
import Day2
import Day3
import Day4


main :: IO ()
main = do 
    -- contents <- readFile "data/day1.txt"
    -- let answer = show $ day1 contents
    -- putStrLn ("day1: " ++ answer)
    -- let answer = show $ day1b contents
    -- putStrLn ("day1b: " ++ answer)


    -- contents <- readFile "data/day2.txt"
    -- let answer = show $ day2 contents
    -- putStrLn ("day2: " ++ answer)
    -- let answer = show $ day2b contents
    -- putStrLn ("day2b: " ++ answer)

    -- contents <- readFile "data/day3.txt"
    -- let answer = show $ day3 contents
    -- putStrLn ("day3: " ++ answer)
    -- let answer = show $ day3b contents
    -- putStrLn ("day3b: " ++ answer)
    contents <- readFile "data/day4.txt"
    let answer = show $ day4 contents
    putStrLn ("day4: " ++ answer)
    -- let answer = show $ day4b contents
    -- putStrLn ("day4b: " ++ answer)