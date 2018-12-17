 module Day10
     ( 
         day10
       , day10b
       , tick
       , parseInput
     ) where
     
import Data.List (sort)
     
input = "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>\n"

day10 input = plotPoints $ head $ drop (day10b input) $ iterate (map move) $ parseInput input

day10b input = (\(a,_,_)->a)
                $ head 
                $ dropWhile (\(a,b,c) -> b>c) 
                $ (\y -> zip3 [0..] y (tail y) ) 
                $ map (gridArea . map fst ) 
                $ iterate (map move) 
                $ parseInput input

tick :: Int -> String -> [String]
tick count input = plotPoints $ head $ drop count $ iterate (map move) $ parseInput input

move :: (Num a, Num b) => ((a, b), (a, b)) -> ((a, b), (a, b))
move ((x,y), (vx, vy)) = ((x + vx, y+vy), (vx, vy))

plotPoints :: (Ord a, Ord b1, Enum b1, Enum a) => [((a, b1), b2)] -> [String]
plotPoints inputData =  reverse 
                            $ map (map (\y -> if y `elem` points then '#' else '.')) 
                            $ getGrid fromXY toXY
    where points = map fst inputData
          fromXY = minXmaxY points
          toXY   = maxXminY points

getGrid :: (Enum b, Enum a) => (a, b) -> (a, b) -> [[(a, b)]]    
getGrid (a,b) (c,d) = [ [(x,y) | x <- [a..c]] | y <- reverse [d..b]]    
          
minXmaxY input = (minimum $ map fst input, maximum $ map snd input)
maxXminY input = (maximum $ map fst input, minimum $ map snd input)

area (a,b) (c,d) = (c-a) * (b-d)

gridArea input = area (minXmaxY input) (maxXminY input)

parseInput :: String -> [((Int, Int), (Int, Int))]
parseInput input = map (\y-> ( 
        read $ '(' : takeWhile (/='>') (drop 10 y) ++")" ,  
        read $ '(' : takeWhile (/='>') (drop 12 $ dropWhile (/='>') y) ++ ")" ) ) $ lines input
