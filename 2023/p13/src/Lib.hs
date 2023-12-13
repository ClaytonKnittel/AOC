module Lib
  ( mirror,
  )
where

import Data.List (findIndex, inits, tails, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

findMirror :: (Eq a) => [[a]] -> Maybe Integer
findMirror =
  fmap ((1 +) . toInteger)
    . findIndex and
    . transpose
    . map
      ( \row ->
          zipWith
            (\l r -> all (uncurry (==)) $ zip l r)
            (map reverse $ init $ tail $ inits row)
            (tail $ tails row)
      )

mirrorIndex :: (Eq a) => [[a]] -> Integer
mirrorIndex rows =
  (fromMaybe 0 . findMirror) rows
    + 100 * (fromMaybe 0 . findMirror $ transpose rows)

parseInput :: String -> [[String]]
parseInput input = splitOn [""] (lines input)

mirror :: IO ()
mirror = do
  input <- readFile "input.txt"
  let grids = parseInput input
   in print (sum $ map mirrorIndex grids)
