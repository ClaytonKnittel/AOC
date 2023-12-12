{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lib
  ( hotSprings,
  )
where

import Data.List (tails)
import Data.List.Split (splitOn)

data Tile = Operational | Damaged | Unknown deriving (Eq, Show)

parseInput :: String -> [([Tile], [Integer])]
parseInput input =
  map
    ( \line ->
        let [springs, contig] = words line
         in ( map
                ( \case
                    '.' -> Operational
                    '#' -> Damaged
                    '?' -> Unknown
                )
                springs,
              map read $ splitOn "," contig
            )
    )
    (lines input)

unfoldLine :: ([Tile], [Integer]) -> ([Tile], [Integer])
unfoldLine (tiles, contigs) =
  ( tiles ++ [Unknown]
      ++ tiles
      ++ [Unknown]
      ++ tiles
      ++ [Unknown]
      ++ tiles
      ++ [Unknown]
      ++ tiles,
    contigs ++ contigs ++ contigs ++ contigs ++ contigs
  )

numWays' :: [Tile] -> [Integer] -> [Integer]
numWays' tiles [] =
  scanr
    ( \tile prevWays ->
        if prevWays == 1 && tile /= Damaged then 1 else 0
    )
    1
    tiles
    ++ [1]
numWays' tiles (contig : contigs) =
  let ways = numWays' tiles contigs
   in foldr
        ( \(withoutWays, subTiles) prevWays ->
            let prevWays2 =
                  if head subTiles /= Damaged
                    then head prevWays
                    else 0
             in if (toInteger (length subTiles) >= contig)
                  && notElem Operational (take (fromInteger contig) subTiles)
                  && ( toInteger (length subTiles) == contig
                         || (subTiles !! fromInteger contig /= Damaged)
                     )
                  then
                    prevWays2
                      + (withoutWays !! fromInteger (contig + 1)) :
                    prevWays
                  else prevWays2 : prevWays
        )
        [0, 0]
        $ init $ zip (init $ tails ways) (tails tiles)

numWays :: [Tile] -> [Integer] -> Integer
numWays tiles contigs = head $ numWays' tiles contigs

hotSprings :: IO ()
hotSprings = do
  input <- readFile "input.txt"
  print (sum (map (uncurry numWays) $ parseInput input))
  print (sum (map (uncurry numWays . unfoldLine) $ parseInput input))
