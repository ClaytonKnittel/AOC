module Main where

import Data.List (sort, sortBy)

valToString :: Integer -> String
valToString 0 = "2"
valToString 1 = "3"
valToString 2 = "4"
valToString 3 = "5"
valToString 4 = "6"
valToString 5 = "7"
valToString 6 = "8"
valToString 7 = "9"
valToString 8 = "T"
valToString 9 = "J"
valToString 10 = "Q"
valToString 11 = "K"
valToString 12 = "A"

stringToVal :: String -> Integer
stringToVal "2" = 0
stringToVal "3" = 1
stringToVal "4" = 2
stringToVal "5" = 3
stringToVal "6" = 4
stringToVal "7" = 5
stringToVal "8" = 6
stringToVal "9" = 7
stringToVal "T" = 8
stringToVal "J" = 9
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

cntRepeats :: [Integer] -> [Integer]
cntRepeats [] = []
cntRepeats [_] = [1]
cntRepeats (a : b : rem) =
  if a == b
    then
      let (bCnt : remCnts) = cntRepeats (b : rem)
       in (bCnt + 1 : remCnts)
    else 1 : cntRepeats (b : rem)

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
