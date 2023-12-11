module Main where

import Data.Map (Map, empty, insert, insertWith, lookup, mapKeys, member, size, toList)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

parseInput :: String -> (Map Integer Integer, Map Integer Integer, Integer)
parseInput input =
  foldr
    ( \(line, y) (xMap, yMap, cnt) ->
        foldr
          ( \(c, x) (xMap, yMap, cnt) -> case c of
              '#' ->
                ( insertWith (+) x 1 xMap,
                  insertWith (+) y 1 yMap,
                  cnt + 1
                )
              _ -> (xMap, yMap, cnt)
          )
          (xMap, yMap, cnt)
          $ zip line [0 ..]
    )
    (empty, empty, 0)
    $ zip (lines input) [0 ..]

remapCoords :: Map Integer Integer -> Integer -> Map Integer Integer
remapCoords map range =
  fst $
    foldr
      ( \val (remap, emptyCnt) ->
          if member val map
            then (insert val (val + emptyCnt) remap, emptyCnt)
            else (remap, emptyCnt + 1)
      )
      (empty, 0)
      (reverse [0 .. (range - 1)])

accumMap :: Map Integer Integer -> Integer -> Integer
accumMap map galaxyCnt =
  fst $
    foldl
      ( \(total, totalCnt) (pos, cnt) ->
          ( total + pos * cnt
              * (2 * totalCnt - galaxyCnt + cnt),
            totalCnt + cnt
          )
      )
      (0, 0)
      (toList map)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let xRange = toInteger $ length (lines input)
      yRange = toInteger $ length (head $ lines input)
      (xMap, yMap, galaxyCnt) = parseInput input
      xRemap = remapCoords xMap xRange
      yRemap = remapCoords yMap yRange
      newXMap = mapKeys (fromJust . (`lookup` xRemap)) xMap
      newYMap = mapKeys (fromJust . (`lookup` yRemap)) yMap
   in print (accumMap newXMap galaxyCnt + accumMap newYMap galaxyCnt)
