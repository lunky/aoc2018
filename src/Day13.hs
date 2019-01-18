 module Day13
     ( 
         day13
       , day13b
       , findNextLoc
       ,DirectionChoice(..)
       ,Cart(..)
       ,moveCart
     ) where
     
import Data.Map.Strict (Map)
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust, isJust)
import Data.List (groupBy,sortBy,sort,foldl')


day13 :: String -> (Int, Int)
day13 input = fromJust $ day13' (Nothing, carts, emptyMap)
    where emptyMap = Map.fromList $ getEmptyMap $ parseInput input
          carts = findCarts $ parseInput input
          
day13b :: String -> (Int,Int)
day13b input = day13b' carts emptyMap
    where emptyMap = Map.fromList $ getEmptyMap $ parseInput input
          carts = findCarts $ parseInput input
          
day13' :: (Maybe (Int, Int), [Cart], Map (Int, Int) Char) -> Maybe (Int, Int)
day13' (collision, carts, emptyMap) 
     | isJust collision = collision
     | otherwise = day13' (dup $ map cartLoc nextCarts, nextCarts, emptyMap)
    where nextCarts = map (`moveCart` emptyMap) carts
     
day13b' ::  [Cart] -> Map (Int, Int) Char  -> (Int,Int)
day13b' carts emptyMap
     | length carts == 1 =  cartLoc $ head carts
     | otherwise = day13b' nextCarts emptyMap
    where nextCarts = map snd  $ Map.toList 
                        $ foldl' (\y x -> moveCartOnMap x y emptyMap) cartMap ( sortBy sort' carts)
          cartMap = getCartMap carts

    
    
data DirectionChoice = Left | Straight | Right 
               deriving (Enum, Bounded, Eq, Show, Ord)

-- add item if we haven't seen it before
dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s 
                           then Just x
                           else dup' xs (Set.insert x s)
                           
removeDups :: [Cart] -> [Cart]
removeDups input  = sortBy sort' $ concat $ filter (\y -> length y == 1) $ groupBy (\(Cart a _ _) (Cart b _ _) -> a==b) $ sortBy sort' input

-- | a `succ` that wraps 
succB :: (Bounded a, Enum a, Eq a) => a -> a 
succB el | el == maxBound = minBound
         | otherwise = succ el

newtype Location = Location (Int,Int)
               deriving (Eq, Show)

data Cart = Cart (Int,Int) Char DirectionChoice 
               deriving (Eq, Show, Ord)

findCarts :: [((Int,Int),Char)] -> [Cart]
findCarts input =  sortBy sort' $ map (\(loc,dir) -> Cart loc dir Day13.Left )  $ filter (\(_,b) -> b `elem` "<>^v"  ) input

sort' (Cart (x1,y1) _ _) (Cart (x2, y2) _ _)
  | y1 > y2 = GT
  | y1 < y2 = LT
  | y1 == y2 = compare x1 x2

filterLoc :: Maybe (Int,Int) -> [Cart] -> [Cart]
filterLoc coord input = case coord of 
                        Nothing -> input 
                        Just c -> filter (\(Cart a _ _) -> a /= c) input

cartLoc :: Cart -> (Int,Int)
cartLoc (Cart a _ _) = a

updateMap :: Map (Int,Int) Char -> [((Int,Int), Char)] -> (Maybe (Int,Int), Map (Int,Int) Char )
updateMap originalMap carts = (Just (0,0), originalMap)
    
getEmptyMap :: [((Int,Int),Char)] -> [((Int,Int),Char)]
getEmptyMap = map (\(key,val) -> (key, replaceCart val))

getCartMap :: [Cart] -> Map (Int,Int) Cart
getCartMap = Map.fromList . map (\ cart@(Cart coord a b)  -> (coord,cart) )


findNextLoc (x,y) '<' = (x-1, y)
findNextLoc (x,y) '>' = (x+1, y)
findNextLoc (x,y) '^' = (x, y-1)
findNextLoc (x,y) 'v' = (x, y+1)
findNextLoc (x,y) _ = error "invalid state"

findNextDir '>' '/' _ = '^'
findNextDir '>' '\\' _ = 'v'
findNextDir '<' '/' _ =  'v'
findNextDir '<' '\\' _ = '^'

findNextDir '^' '\\' _ = '<'
findNextDir '^' '/' _ =  '>'

findNextDir 'v' '\\' _ = '>'
findNextDir 'v' '/' _ =  '<'

{--

/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   

--}


findNextDir '>' '+' Day13.Left = '^'
findNextDir '>' '+' Day13.Right= 'v'

findNextDir '<' '+' Day13.Left = 'v'
findNextDir '<' '+' Day13.Right = '^'

findNextDir '^' '+' Day13.Left = '<'
findNextDir '^' '+' Day13.Right = '>'

findNextDir 'v' '+' Day13.Left = '>'
findNextDir 'v' '+' Day13.Right = '<'
findNextDir a '+' Day13.Straight = a
findNextDir a _ _ = a

--  data Cart = Cart (Int,Int) Char DirectionChoice 

moveCart :: Cart -> Map (Int,Int) Char -> Cart
moveCart (Cart (x,y) currDirection choice) emptyMap = Cart nextLoc nextDir nextChoice
    where nextLoc = findNextLoc (x,y) currDirection
          elementInNextloc = emptyMap Map.! nextLoc
          nextDir = findNextDir currDirection elementInNextloc choice
          nextChoice = case elementInNextloc of '+'       -> succB choice
                                                _ -> choice
moveCartOnMap cart cartMap emptyMap = do
    let (Cart curLoc a b) = cart
    let newCart @ (Cart newLoc a b) = moveCart cart emptyMap
    if not (Map.member curLoc cartMap) then
        cartMap
    else
        if Map.member newLoc cartMap  then
            Map.delete curLoc $ Map.delete newLoc cartMap 
        else
            Map.delete curLoc $ Map.insert newLoc newCart cartMap 


replaceCart '<' = '-'
replaceCart '>' = '-'
replaceCart '^' = '|'
replaceCart 'v' = '|'
replaceCart a = a


parseInput input =  concatMap ((\(y, pts) -> map (\ (x,val) -> ((x, y), val)) pts) 
                            . (\(y, pts) -> (y, zip [0 ..] pts))) (zip [0 ..] $ lines input)

-- input =  "|\nv\n|\n|\n|\n^\n|"
--input = "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "
input = "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"
carts = findCarts $ parseInput input
emptyMap = Map.fromList $ getEmptyMap $ parseInput input
cartMap = getCartMap carts

