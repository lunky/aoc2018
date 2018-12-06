{-# LANGUAGE ViewPatterns #-}
 module Day4
     ( 
        day4
       ,day4b
     ) where

import Data.List (sortBy, sort, foldl', minimumBy)
import Data.Function (on)
import Data.Functor
import Data.Void (Void)
import Text.Megaparsec


import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Arrow (first, second, (&&&))

type Parser s = Parsec Void s
type MonadChar e s m = (MonadParsec e s m, Token s ~ Char)

data GuardShift = WakeUp | Sleep | Begin Int
  deriving (Eq, Ord, Show)

data LogRecord =
  LogRecord { month :: Int, day :: Int, hour :: Int, mins :: Int, info :: GuardShift }
  deriving (Eq, Ord, Show)

input = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"

justParse :: (Show (Token s), Show e) => Parsec e s a -> s -> a
justParse p s = case parse p "" s of
  Left x -> error (show x)
  Right a -> a
  
guardShiftP :: Parser String GuardShift
guardShiftP =

  (C.string "falls asleep" $> Sleep)
  <|> (C.string "wakes up" $> WakeUp)
  <|> (C.string "Guard #" *> (Begin <$> signedIntP) <* takeWhile1P Nothing (const True))

recordP :: Parser String LogRecord
recordP =
  LogRecord
  <$> (C.string "[1518-" *> signedIntP)
  <*> (C.char '-' *> signedIntP)
  <*> (C.space1 *> signedIntP)
  <*> (C.char ':' *> signedIntP)
  <*> (C.string "] " *> guardShiftP)
  
signedIntP :: (MonadChar e s m, Integral a) => m a
signedIntP = L.signed space L.decimal

space :: MonadChar e s m => m ()
space = L.space C.space1 empty empty


day4 input =   (\(x,(y,_))-> x * y)
               $ second (minimumBy (flip compare `on` snd) . snd)
               $ minimumBy (flip compare `on` snd)
                    (map (second (\ (HM.toList -> xs) -> (sum $ map snd xs, xs))) 
                   $ HM.toList
                   $ (\(x, y, z) -> x) 
                   $ foldl' handle (HM.empty, Nothing, Nothing) 
                   $ sort 
                   $ map (justParse recordP) 
                   $ lines input)
                where
                    minsBetween :: LogRecord -> LogRecord -> [Int]
                    minsBetween sleep wake = [mins sleep .. mins wake - 1]
                    waken i sleepinfo wakeinfo hm =
                      foldl' (\h m -> HM.alter
                             (\case {Nothing -> Just (HM.fromList [(m, 1)]);
                                     Just g -> Just (HM.alter 
                                                        (\case {  Nothing -> Just 1;
                                                                  Just i -> Just (i + 1);}) m g)
                                    }
                             ) i h) hm (minsBetween sleepinfo wakeinfo)
                    handle (h, prev, cur) logRecord@LogRecord{info} = case info of
                      Begin i -> (h, Nothing, Just i)
                      Sleep -> (h, Just logRecord, cur)
                      WakeUp -> case (prev, cur) of
                          (Just p, Just i) -> (waken i p logRecord h, Nothing, Just i)
                          _ -> error "How did I get here"
                

day4b:: String -> Int
day4b input = uncurry (*) 
                $ fst 
                $ minimumBy (flip compare `on` snd)
                    (HM.toList 
                        $ (\ (x, y, z) -> x) 
                        $ foldl' go (HM.empty, Nothing, Nothing) 
                        $ sort 
                        $ map (justParse recordP) 
                        $ lines input)
  where
    minsBetween :: LogRecord -> LogRecord -> [Int]
    minsBetween sleep wake = [mins sleep .. mins wake - 1]
    process i sleepinfo wakeinfo hm =
      foldl' (\h m -> HM.alter (\case {Nothing -> Just 1; Just k -> Just (k + 1)}) (i, m) h) 
            hm (minsBetween sleepinfo wakeinfo)
    go (h, prev, cur) record@LogRecord{info} = case info of
      Begin i -> (h, Nothing, Just i)
      Sleep -> (h, Just record, cur)
      WakeUp -> case (prev, cur) of
        (Just p, Just i) -> (process i p record h, Nothing, Just i)
        _ -> error "How did I get here"
    