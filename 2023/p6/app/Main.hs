module Main where

intSqrt :: Integer -> Integer
intSqrt = floor . sqrt . fromInteger

minTimeToHold :: Integer -> Integer -> Integer
minTimeToHold t d = 1 + floor ((fromInteger t - (sqrt . fromInteger) (t ^ 2 - 4 * d)) / 2)

waysToBeat :: Integer -> Integer -> Integer
waysToBeat t d =
  if 4 * d >= t ^ 2
    then 0
    else
      let minTime = minTimeToHold t d
       in t - 2 * minTime + 1

parseTimes :: String -> [Integer]
parseTimes input =
  case words input of
    ("Time:" : times) -> map read times

parseTimes2 :: String -> Integer
parseTimes2 input =
  case words input of
    ("Time:" : times) -> read (concat times)

parseDistances :: String -> [Integer]
parseDistances input =
  case words input of
    ("Distance:" : distances) -> map read distances

parseDistances2 :: String -> Integer
parseDistances2 input =
  case words input of
    ("Distance:" : distances) -> read (concat distances)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let [timesInput, distancesInput] = lines contents
      -- times = parseTimes timesInput
      time = parseTimes2 timesInput
      -- dists = parseDistances distancesInput
      dist = parseDistances2 distancesInput
   in print (waysToBeat time dist)

-- in print (product (zipWith waysToBeat times dists))
