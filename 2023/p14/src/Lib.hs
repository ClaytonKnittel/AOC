{-# LANGUAGE LambdaCase #-}

module Lib
  ( rockTilt,
  )
where

import Data.List (genericLength, transpose)
import Data.Maybe (fromMaybe)

data Tile = O | S | Empty

instance Show Tile where
  show = \case
    O -> "O"
    S -> "#"
    Empty -> "."

printTiles :: [[Tile]] -> IO ()
printTiles = mconcat . map (putStrLn . concatMap show)

parseTile :: Char -> Maybe Tile
parseTile 'O' = Just O
parseTile '#' = Just S
parseTile '.' = Just Empty
parseTile _ = Nothing

-- Computes the total load of rocks in the column of tiles
-- after sliding all circular rocks as far north as possible
-- (toward the beginning of the list), not passing square
-- rocks.
rockTiltLoadCol :: [Tile] -> Integer
rockTiltLoadCol = tiltLoad <*> genericLength
  where
    tiltLoad [] = const 0
    tiltLoad (tile : tiles) = case tile of
      O -> (+) <*> (tiltLoad tiles . (-1 +))
      S -> const $ rockTiltLoadCol tiles
      Empty -> tiltLoad tiles

rockTiltLoad :: [[Tile]] -> Integer
rockTiltLoad = sum . map rockTiltLoadCol . transpose

parseInput :: String -> Maybe [[Tile]]
parseInput = mapM (mapM parseTile) . lines

rockTilt :: IO ()
rockTilt =
  do
    input <- readFile "input.txt"
    let rocks = fromMaybe [[]] $ parseInput input
     in printTiles rocks
          <> print (rockTiltLoad rocks)
