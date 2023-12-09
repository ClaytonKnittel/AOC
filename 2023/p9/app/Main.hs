module Main where

parseLine :: String -> [Integer]
parseLine = map read . words

parseInput :: String -> [[Integer]]
parseInput = map parseLine . lines

predictNext :: [Integer] -> Integer
predictNext measurements
  | all (== 0) measurements = 0
  | otherwise =
    last measurements
      + predictNext
        ( zipWith
            (-)
            (tail measurements)
            measurements
        )

main :: IO ()
main = do
  input <- readFile "input.txt"
  print (sum (map predictNext (parseInput input)))
