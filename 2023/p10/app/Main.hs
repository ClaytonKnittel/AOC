module Main where

import Data.Map (Map, empty, insert, lookup, mapMaybe)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

addMaybe :: Integer -> Maybe Integer -> Maybe Integer
addMaybe n (Just i) = Just (i + n)
addMaybe _ Nothing = Nothing

incMaybe :: Maybe Integer -> Maybe Integer
incMaybe = addMaybe 1

data Point = Point Integer Integer deriving (Eq, Ord, Show)

pointX :: Point -> Integer
pointX (Point x _) = x

pointY :: Point -> Integer
pointY (Point _ y) = y

pointNeighbors :: Point -> [Point]
pointNeighbors (Point x y) =
  [ Point (x + 1) y,
    Point x (y + 1),
    Point (x -1) y,
    Point x (y -1)
  ]

data Tile = V | H | NE | NW | SE | SW | G | S deriving (Show)

mkTile :: Char -> Tile
mkTile '.' = G
mkTile '|' = V
mkTile '-' = H
mkTile 'L' = NE
mkTile 'J' = NW
mkTile '7' = SW
mkTile 'F' = SE
mkTile 'S' = S

tileConnection :: Point -> Point -> Tile -> Maybe Point
tileConnection (Point fromX fromY) (Point toX toY) tile
  | toX - fromX == 1 = case tile of
    NW -> Just $ Point toX (toY - 1)
    H -> Just $ Point (toX + 1) toY
    SW -> Just $ Point toX (toY + 1)
    _ -> Nothing
  | toX - fromX == -1 = case tile of
    NE -> Just $ Point toX (toY - 1)
    H -> Just $ Point (toX - 1) toY
    SE -> Just $ Point toX (toY + 1)
    _ -> Nothing
  | toY - fromY == 1 = case tile of
    NW -> Just $ Point (toX - 1) toY
    V -> Just $ Point toX (toY + 1)
    NE -> Just $ Point (toX + 1) toY
    _ -> Nothing
  | toY - fromY == -1 = case tile of
    SW -> Just $ Point (toX - 1) toY
    V -> Just $ Point toX (toY - 1)
    SE -> Just $ Point (toX + 1) toY
    _ -> Nothing

parseInput :: String -> (Maybe Point, Map Point Tile)
parseInput input =
  foldr
    ( \(c, p) (start, map) -> case mkTile c of
        G -> (start, map)
        S -> (Just p, insert p S map)
        tile -> (start, insert p tile map)
    )
    (Nothing, empty)
    $ concat
      ( zipWith
          (\line y -> zipWith (\c x -> (c, Point x y)) line [0 ..])
          (lines input)
          [0 ..]
      )

followMaze :: Point -> Point -> Map Point Tile -> Maybe Integer
followMaze from to map =
  case lookup to map of
    Just S -> Just 1
    Just tile -> case tileConnection from to tile of
      Just nextTile -> incMaybe $ followMaze to nextTile map
      _ -> Nothing
    _ -> Nothing

computeInteriorSize' :: Point -> Point -> Point -> Map Point Tile -> (Maybe Integer, Maybe Integer)
computeInteriorSize' start from to map =
  case lookup to map of
    Just S -> (Just 0, Just 1)
    Just tile -> case tileConnection from to tile of
      Just nextTile ->
        let (area, perim) = computeInteriorSize' start to nextTile map
         in ( addMaybe
                ( (pointY nextTile - pointY to)
                    * (pointX to - pointX start)
                )
                area,
              incMaybe perim
            )
      _ -> (Nothing, Nothing)
    _ -> (Nothing, Nothing)

computeInteriorSize ::
  Point ->
  Point ->
  Map Point Tile ->
  Maybe Integer
computeInteriorSize start to map =
  case computeInteriorSize' start start to map of
    (Just area, Just perim)
      | area >= 0 ->
        Just (area - (perim `div` 2 - 1))
      | otherwise -> Nothing
    (Nothing, Nothing) -> Nothing

-- case tileConnection from to

maxLen :: Maybe Integer -> Integer
maxLen (Just len) = len `div` 2
maxLen Nothing = -1

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (Just start, tileMap) = parseInput input
   in print
        ( maximum $
            map
              (\neighbor -> maxLen $ followMaze start neighbor tileMap)
              (pointNeighbors start)
        )
        <> print
          ( maximum $
              map
                ( \neighbor ->
                    fromMaybe (-1) $
                      computeInteriorSize start neighbor tileMap
                )
                (pointNeighbors start)
          )
