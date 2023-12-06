module Main where

import Data.Set (Set, empty, insert, intersection, size, toList)

data Card = Card {number :: Integer, winning, nums :: Set Integer}

instance Show Card where
  show Card {number = n, winning = w, nums = nums} = "Card " ++ show n ++ ": " ++ show (toList w) ++ " | " ++ show (toList nums)

cardScore :: Card -> Integer
cardScore Card {winning = w, nums = n} =
  let overlap = w `intersection` n
   in case size overlap of
        0 -> 0
        n -> 2 ^ (n - 1)

parseMyNums :: [String] -> Card
parseMyNums [] = Card {number = 0, winning = empty, nums = empty}
parseMyNums (num : nums) =
  let card@Card {nums = n} = parseMyNums nums
   in card {nums = insert (read num) n}

parseWinningNums :: [String] -> Card
parseWinningNums ("|" : nums) = parseMyNums nums
parseWinningNums (num : nums) =
  let card@Card {winning = w} = parseWinningNums nums
   in card {winning = insert (read num) w}

parseLine :: String -> Card
parseLine s = case words s of
  ("Card" : numWithColon : rem) -> (parseWinningNums rem) {number = read (init numWithColon) :: Integer}

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (sum (map (cardScore . parseLine) (lines contents)))
