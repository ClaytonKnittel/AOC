module Main where

import Data.Set (Set, empty, insert, intersection, size, toList)

data Card = Card {number :: Integer, winning, nums :: Set Integer}

instance Show Card where
  show Card {number = n, winning = w, nums = nums} = "Card " ++ show n ++ ": " ++ show (toList w) ++ " | " ++ show (toList nums)

cardOverlap :: Card -> Integer
cardOverlap Card {winning = w, nums = n} = (toInteger . size) (w `intersection` n)

overlapScore :: Integer -> Integer
overlapScore overlap =
  case overlap of
    0 -> 0
    n -> 2 ^ (n - 1)

cardScore :: Card -> Integer
cardScore = overlapScore . cardOverlap

cardCountP2 :: [Card] -> [Integer]
cardCountP2 [] = []
cardCountP2 (card : cards) =
  let counts = cardCountP2 cards
      overlap = (fromIntegral . cardOverlap) card
   in (1 + sum (take overlap counts) : counts)

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
  -- print (sum (map (cardScore . parseLine) (lines contents)))
  print ((sum . cardCountP2 . map parseLine) (lines contents))
