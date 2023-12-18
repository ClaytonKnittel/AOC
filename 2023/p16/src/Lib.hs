{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( lava,
  )
where

import Data.Bool (bool)
import Data.Map (Map, empty, fromList, insertWith, keys, lookup)
import Prelude hiding (Left, Right, lookup)

type CMap = Map (Integer, Integer)

data Tile = VSplit | HSplit | FMirror | RMirror | Empty

parseTile :: Char -> Tile
parseTile '|' = VSplit
parseTile '-' = HSplit
parseTile '/' = FMirror
parseTile '\\' = RMirror
parseTile '.' = Empty

instance Show Tile where
  show VSplit = "|"
  show HSplit = "-"
  show FMirror = "/"
  show RMirror = "\\"
  show Empty = "."

data Direction = Up | Down | Left | Right deriving (Eq)

instance Show Direction where
  show Up = "^"
  show Down = "v"
  show Left = "<"
  show Right = ">"

nextCoord :: (Integer, Integer) -> Direction -> (Integer, Integer)
nextCoord (x, y) Up = (x, y - 1)
nextCoord (x, y) Down = (x, y + 1)
nextCoord (x, y) Left = (x - 1, y)
nextCoord (x, y) Right = (x + 1, y)

reflTile :: Direction -> Tile -> [Direction]
reflTile Up HSplit = [Left, Right]
reflTile Up FMirror = [Right]
reflTile Up RMirror = [Left]
reflTile Down HSplit = [Left, Right]
reflTile Down FMirror = [Left]
reflTile Down RMirror = [Right]
reflTile Left VSplit = [Up, Down]
reflTile Left FMirror = [Down]
reflTile Left RMirror = [Up]
reflTile Right VSplit = [Up, Down]
reflTile Right FMirror = [Up]
reflTile Right RMirror = [Down]
reflTile dir _ = [dir]

nextTiles :: (Integer, Integer) -> Direction -> Tile -> [((Integer, Integer), Direction)]
nextTiles coord dir = map (nextCoord coord >>= (,)) . reflTile dir

parseInput :: String -> CMap Tile
parseInput =
  fromList
    . concat
    . zipWith
      ( (`zipWith` [0 ..])
          . (((. parseTile) . (,)) .)
          . flip (,)
      )
      [0 ..]
    . lines

follow :: (Integer, Integer) -> Direction -> CMap Tile -> CMap [Direction] -> CMap [Direction]
follow coord dir tiles dirs =
  maybe
    dirs
    ( foldr
        ( \(newCoord, newDir) newDirs ->
            follow newCoord newDir tiles $
              insertWith (++) coord [dir] newDirs
        )
        dirs
        . nextTiles coord dir
    )
    ( bool
        (lookup coord tiles)
        Nothing
        (maybe False (elem dir) (lookup coord dirs))
    )

followPath :: CMap Tile -> CMap [Direction]
followPath tiles = follow (0, 0) Right tiles empty

maximizeEnergy :: CMap Tile -> Integer
maximizeEnergy tiles =
  let xr = maximum $ map fst (keys tiles)
      yr = maximum $ map snd (keys tiles)
   in maximum $
        map
          ( ( \(coord, dir) ->
                toInteger $
                  length $ follow coord dir tiles empty
            )
              . (,Right)
              . (0,)
          )
          [0 .. yr]
          ++ map
            ( ( \(coord, dir) ->
                  toInteger $
                    length $ follow coord dir tiles empty
              )
                . (,Left)
                . (xr,)
            )
            [0 .. yr]
          ++ map
            ( ( \(coord, dir) ->
                  toInteger $
                    length $ follow coord dir tiles empty
              )
                . (,Down)
                . (,0)
            )
            [0 .. xr]
          ++ map
            ( ( \(coord, dir) ->
                  toInteger $
                    length $ follow coord dir tiles empty
              )
                . (,Up)
                . (,yr)
            )
            [0 .. xr]

lava :: IO ()
lava =
  do
    input <- readFile "input.txt"
    let tiles = parseInput input
        dirs = followPath tiles
     in -- xr = maximum $ map fst (keys dirs)
        -- yr = maximum $ map snd (keys dirs)
        print (length dirs)
          <> print (maximizeEnergy tiles)

-- <> mconcat
--   ( map
--       ( \y ->
--           putStrLn
--             ( concatMap
--                 ( \x ->
--                     case lookup (x, y) $ parseInput input of
--                       Just Empty ->
--                         maybe
--                           "."
--                           ( \case
--                               [dir] -> show dir
--                               dirs2 -> show (length dirs2)
--                           )
--                           (lookup (x, y) dirs)
--                       Just mirror -> show mirror
--                 )
--                 [0 .. xr]
--             )
--       )
--       [0 .. yr]
--   )
