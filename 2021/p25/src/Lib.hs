{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( cucumberMoves,
  )
where

import Control.Monad.State
import Data.List (intercalate, tails, transpose)
import Prelude hiding (rem)

data Tile = E | S | Empty deriving (Eq)

instance Show Tile where
  show E = ">"
  show S = "v"
  show Empty = "."

data ChangeState a = ChangeState {value :: a, flag :: Bool}

instance Functor ChangeState where
  fmap fn ChangeState {value = val, flag = f} =
    ChangeState {value = fn val, flag = f}

instance Applicative ChangeState where
  pure val = ChangeState {value = val, flag = False}

  ChangeState {value = fn, flag = f1}
    <*> ChangeState {value = a, flag = f2} =
      ChangeState {value = fn a, flag = f1 || f2}

instance Monad ChangeState where
  return = pure

  ChangeState {value = v1, flag = f1} >>= fn =
    let ChangeState {value = v2, flag = f2} = fn v1
     in ChangeState {value = v2, flag = f1 || f2}

changedState :: a -> ChangeState a
changedState val = ChangeState {value = val, flag = True}

-- type Changed a = State ChangeState a

printTiles :: [[Tile]] -> String
printTiles = intercalate "\n" . map (concatMap show)

toTile :: Char -> Tile
toTile '>' = E
toTile 'v' = S
toTile '.' = Empty

transposeTile :: Tile -> Tile
transposeTile E = S
transposeTile S = E
transposeTile Empty = Empty

parseInput :: String -> [[Tile]]
parseInput = map (map toTile) . lines

slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n lst =
  take (length lst) $
    map (take n) $ tails $ cycle lst

rotateList :: [a] -> [a]
rotateList [] = []
rotateList x = last x : init x

moveRowRight :: [Tile] -> ChangeState [Tile]
moveRowRight = fmap rotateList . mapM nextItem . slidingWindows 3
  where
    nextItem [E, Empty, _] = changedState E
    nextItem [_, E, Empty] = changedState Empty
    nextItem [_, b, _] = return b

transposeGrid :: [[Tile]] -> [[Tile]]
transposeGrid = map (map transposeTile) . transpose

step :: [[Tile]] -> ChangeState [[Tile]]
step tiles = do
  r1 <- mapM moveRowRight tiles
  let t1 = transposeGrid r1
  r2 <- mapM moveRowRight t1
  return (transposeGrid r2)

stepsUntilEqual :: [[Tile]] -> Integer
stepsUntilEqual tiles =
  (+ 1) $
    toInteger $
      length $
        takeWhile (uncurry (/=)) $ zip steps (tail steps)
  where
    steps = iterate (value . step) tiles

stepsUntilEqual2 :: [[Tile]] -> Integer
stepsUntilEqual2 tiles =
  toInteger $
    length $
      takeWhile flag steps
  where
    steps = iterate (step . value) (changedState tiles)

cucumberMoves :: IO ()
cucumberMoves = do
  input <- readFile "input.txt"
  let grid = parseInput input
   in print (stepsUntilEqual grid)

-- print (stepsUntilEqual2 grid)

-- <> print (stepsUntilEqual2 grid)
