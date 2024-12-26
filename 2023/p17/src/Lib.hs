module Lib
  ( lavaPath,
  )
where

import Control.Monad (join)
import Data.Bool (bool)
import Data.Map (Map, empty, fromList, insert, keys, lookup, member)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right, lookup)

fib :: Int -> Integer
fib = (map f [0 ..] !!)
  where
    f 0 = 0
    f 1 = 1
    f n = fib (n -2) + fib (n - 1)

data Direction = Up | Down | Left | Right deriving (Eq, Ord)

eachDir :: [Direction]
eachDir = [Left, Down, Right, Up]

oppositeDir :: Direction -> Direction
oppositeDir Up = Down
oppositeDir Down = Up
oppositeDir Left = Right
oppositeDir Right = Left

dDir :: Direction -> (Integer, Integer)
dDir Up = (-1, 0)
dDir Down = (1, 0)
dDir Left = (-1, 0)
dDir Right = (1, 0)

addCoord :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type CMap = Map (Integer, Integer)

maxCoord :: Map (Integer, Integer) a -> (Integer, Integer)
maxCoord = foldr max (0, 0) . keys

type MMap = Map (Integer, Integer, Direction, Integer)

shortestPath :: CMap Integer -> Integer
shortestPath m =
  let (x, y) = maxCoord m
   in fromMaybe (-1) $ join (lookup (x, y, Down, 1) $ path (x, y) Down 1 m empty)
  where
    -- coord, direction entering tile, distance traveled in same direction,
    -- tile map, mem map
    path ::
      (Integer, Integer) ->
      Direction ->
      Integer ->
      CMap Integer ->
      MMap (Maybe Integer) ->
      MMap (Maybe Integer)
    path _ _ 4 _ mem = mem
    path (0, 0) dir dist _ mem = insert (0, 0, dir, dist) (Just 0) mem
    path (x, y) dir dist tiles mem =
      bool
        ( snd $
            foldr
              ( \dir2 (dist2, mem2) ->
                  maybe
                    ( insert
                        (x, y, dir, dist)
                        Nothing
                        mem2
                    )
                    (const mem2)
                    ( lookup
                        (x, y, dir, dist)
                        mem2
                    )
              )
              (Nothing, mem)
              (filter (/= oppositeDir dir) eachDir)
        )
        mem
        $ member (x, y, dir, dist) mem

--fromMaybe
--  ( minimum $
--      map
--        ( \dir2 ->
--            path
--              (dDir dir2)
--              dir2
--              (bool 1 (dist + 1) $ dir == dir2)
--              tiles
--              $ insert (x, y, dir, dist) Nothing mem
--        )
--        (filter (/= oppositeDir dir) eachDir)
--  )
--  $ lookup (x, y, dir, dist) mem

parseInput :: String -> CMap Integer
parseInput =
  fromList
    . concat
    . zipWith
      ( (`zipWith` [0 ..])
          . (((. read . (: [])) . (,)) .)
          . flip (,)
      )
      [0 ..]
    . lines

lavaPath :: IO ()
lavaPath =
  do
    input <- readFile "input.txt"
    let tiles = parseInput input
     in print tiles
          <> print (shortestPath tiles)
