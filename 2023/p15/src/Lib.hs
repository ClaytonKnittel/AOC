{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( addSteps,
  )
where

import Data.Bool (bool)
import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Map (Map, assocs, empty, findWithDefault, insert, insertWith, lookup)
import Prelude hiding (lookup)

hash :: Char -> Integer -> Integer
hash = ((((`mod` 256) . (* 17)) .) . (+)) . (toInteger . ord)

data Op = Remove String Integer | Remap Integer String Integer

parseOp :: String -> Op
parseOp opStr = parse opStr "" 0
  where
    parse :: String -> String -> Integer -> Op
    parse ['-'] = Remove
    parse ('=' : focalLength) = Remap (read focalLength)
    parse (c : cs) = (. hash c) . parse cs . (++ [c])

applyOp :: Op -> Map Integer [(String, Integer)] -> Map Integer [(String, Integer)]
applyOp (Remove str idx) m =
  insert
    idx
    ( filter
        ((/= str) . fst)
        (findWithDefault [] idx m)
    )
    m
applyOp (Remap focalLength str idx) m =
  bool
    ( insertWith
        (flip (++))
        idx
        [(str, focalLength)]
        (applyOp (Remove str idx) m)
    )
    ( insert
        idx
        ( map
            (bool (str, focalLength) <*> ((/= str) . fst))
            (findWithDefault [] idx m)
        )
        m
    )
    $ maybe False (any ((== str) . fst)) (lookup idx m)

focusingPower :: Map Integer [(String, Integer)] -> Integer
focusingPower =
  sum
    . map
      ( uncurry $
          (. (sum . zipWith (flip $ (*) . snd) [1 ..]))
            . (*)
            . (+ 1)
      )
    . assocs

addSteps :: IO ()
addSteps =
  do
    input <- readFile "input.txt"
    print $ sum $ map (foldl (flip hash) 0) $ splitOn "," $ filter (/= '\n') input
    print $ focusingPower $ foldl (flip $ applyOp . parseOp) empty $ splitOn "," $ filter (/= '\n') input
