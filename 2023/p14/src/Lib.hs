{-# LANGUAGE LambdaCase #-}

module Lib
  ( rockTilt,
  )
where

import Data.Bool (bool)
import Data.List (find, genericLength, transpose)
import Data.Map (Map, assocs, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

data Tile = O | S | Empty deriving (Eq, Ord)

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

rockTiltLoadColCnst :: [Tile] -> Integer
rockTiltLoadColCnst =
  sum
    . flip
      (zipWith $ bool (const 0) id . (== O))
      [1 ..]
    . reverse

-- rockTiltLoadColCnst = sum . zipWith (\x t -> bool 0 x (t == O)) [1 ..]

rockTiltLoadCnst :: [[Tile]] -> Integer
rockTiltLoadCnst = sum . map rockTiltLoadColCnst

-- Slides all O rocks toward the beginning of the list
-- without passing any S rocks.
shiftRocks :: [Tile] -> [Tile]
shiftRocks = flip shiftR []
  where
    shiftR [] = ([] ++)
    shiftR (tile : tiles) = case tile of
      O -> (tile :) . shiftR tiles
      S -> (++ S : shiftRocks tiles)
      Empty -> shiftR tiles . ([Empty] ++)

cwRotateGrid :: [[a]] -> [[a]]
cwRotateGrid = map reverse . transpose

ccwRotateGrid :: [[a]] -> [[a]]
ccwRotateGrid = transpose . map reverse

runCycle :: [[Tile]] -> [[Tile]]
runCycle = (!! 4) . iterate (cwRotateGrid . map shiftRocks)

stateAfterNSteps :: (Ord a) => (a -> a) -> a -> Integer -> a
stateAfterNSteps = stepN empty
  where
    stepN :: (Ord a) => Map a Integer -> (a -> a) -> a -> Integer -> a
    stepN _ _ state 0 = state
    stepN m fn state steps' =
      maybe
        ( stepN
            (insert state steps' m)
            fn
            (fn state)
            (steps' - 1)
        )
        ( \x ->
            maybe
              state
              fst
              $ find ((== x - (steps' `mod` (x - steps'))) . snd) (assocs m)
        )
        (lookup state m)

parseInput :: String -> Maybe [[Tile]]
parseInput = mapM (mapM parseTile) . lines

rockTilt :: IO ()
rockTilt =
  do
    input <- readFile "input.txt"
    let rocks = fromMaybe [[]] $ parseInput input
     in printTiles rocks
          <> print (rockTiltLoad rocks)
          <> print
            ( rockTiltLoadCnst $
                stateAfterNSteps
                  runCycle
                  (ccwRotateGrid rocks)
                  1000000000
            )
