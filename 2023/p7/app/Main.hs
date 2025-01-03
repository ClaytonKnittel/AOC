{-# LANGUAGE BlockArguments #-}

module Main where

import Data.List (sort, sortBy)
import Data.Ord (Down (Down), comparing)

valToString :: Integer -> String
valToString 1 = "2"
valToString 2 = "3"
valToString 3 = "4"
valToString 4 = "5"
valToString 5 = "6"
valToString 6 = "7"
valToString 7 = "8"
valToString 8 = "9"
valToString 9 = "T"
valToString 0 = "J"
valToString 10 = "Q"
valToString 11 = "K"
valToString 12 = "A"

stringToVal :: String -> Integer
stringToVal "2" = 1
stringToVal "3" = 2
stringToVal "4" = 3
stringToVal "5" = 4
stringToVal "6" = 5
stringToVal "7" = 6
stringToVal "8" = 7
stringToVal "9" = 8
stringToVal "T" = 9
stringToVal "J" = 0
stringToVal "Q" = 10
stringToVal "K" = 11
stringToVal "A" = 12

newtype Hand = Hand (Integer, Integer, Integer, Integer, Integer)

instance Show Hand where
  show (Hand (c1, c2, c3, c4, c5)) =
    valToString c1
      ++ valToString c2
      ++ valToString c3
      ++ valToString c4
      ++ valToString c5

-- Skip Js here
cntRepeats' :: [Integer] -> [Integer]
cntRepeats' [] = []
cntRepeats' (0 : rem) = cntRepeats' rem
cntRepeats' [_] = [1]
cntRepeats' (a : b : rem) =
  if a == b
    then
      let (bCnt : remCnts) = cntRepeats' (b : rem)
       in (bCnt + 1 : remCnts)
    else 1 : cntRepeats' (b : rem)

cntRepeats :: [Integer] -> [Integer]
cntRepeats nums =
  let numJacks = toInteger (length (filter (== 0) nums))
      reps = sortBy (comparing Data.Ord.Down) (cntRepeats' nums)
   in case reps of
        [] -> [numJacks]
        (mostFreq : rem) -> mostFreq + numJacks : rem

reduceBase' :: [Integer] -> Integer -> (Integer, Integer)
reduceBase' [] _ = (0, 1)
reduceBase' (n : rem) base =
  let (val, mult) = reduceBase' rem base
   in (n * mult + val, mult * base)

reduceBase :: [Integer] -> Integer -> Integer
reduceBase num base = fst (reduceBase' num base)

classifyList :: [Integer] -> Integer
classifyList [5] = 6
classifyList [1, 4] = 5
classifyList [2, 3] = 4
classifyList [1, 1, 3] = 3
classifyList [1, 2, 2] = 2
classifyList [1, 1, 1, 2] = 1
classifyList [1, 1, 1, 1, 1] = 0

handScore :: Hand -> Integer
handScore (Hand (c1, c2, c3, c4, c5)) =
  let handClass = (classifyList . sort . cntRepeats . sort) [c1, c2, c3, c4, c5]
   in reduceBase [handClass, c1, c2, c3, c4, c5] 13

parseHand :: String -> Hand
parseHand [c1, c2, c3, c4, c5] =
  Hand
    ( stringToVal [c1],
      stringToVal [c2],
      stringToVal [c3],
      stringToVal [c4],
      stringToVal [c5]
    )

parseLine :: String -> (Hand, Integer)
parseLine line =
  let [handStr, bidStr] = words line
   in (parseHand handStr, read bidStr)

parseInput :: String -> [(Hand, Integer)]
parseInput = map parseLine . lines

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print
    ( sum
        ( zipWith
            (curry (\((_, bid), rank) -> bid * rank))
            ( sortBy
                (\h1 h2 -> handScore (fst h1) `compare` handScore (fst h2))
                (parseInput contents)
            )
            [1 ..]
        )
    )
