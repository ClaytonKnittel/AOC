{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (genericLength)
import Data.Map (Map, fromList, keys, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)

data Direction = Left | Right

data Node = Node {left, right :: String}

parseInstructions :: String -> [Direction]
parseInstructions =
  map
    ( \case
        'L' -> Left
        'R' -> Right
    )

parseConnection :: String -> (String, Node)
parseConnection line =
  let [key, _, _ : l, r] = words line
   in (key, Node {left = init l, right = init r})

parseInput :: String -> ([Direction], Map String Node)
parseInput input =
  let (directions : _ : connections) = lines input
   in ( parseInstructions directions,
        fromList (map parseConnection connections)
      )

computePath :: String -> (String -> Bool) -> [Direction] -> Map String Node -> [String]
computePath startNode endCondition directions connections =
  takeWhile
    endCondition
    ( scanl
        ( \curNodeName direction ->
            let node =
                  fromMaybe
                    (Node {left = "", right = ""})
                    (Data.Map.lookup curNodeName connections)
             in case direction of
                  Left -> left node
                  Right -> right node
        )
        startNode
        (cycle directions)
    )

countPathLength :: String -> (String -> Bool) -> [Direction] -> Map String Node -> Integer
countPathLength startNode endCondition directions = genericLength . computePath startNode endCondition directions

lcmList :: [Integer] -> Integer
lcmList [n] = n
lcmList (a : b : rem) = lcmList (lcm a b : rem)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (directions, connections) = parseInput input
   in print
        ( lcmList
            ( map
                ( \startNode ->
                    countPathLength
                      startNode
                      ('Z' `notElem`)
                      directions
                      connections
                )
                (filter ('A' `elem`) (keys connections))
            )
        )

-- in print (countPathLength "AAA" (/= "ZZZ") directions connections)
-- in print (computePath "AAA" (/= "ZZZ") directions connections)
