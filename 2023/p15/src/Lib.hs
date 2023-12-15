module Lib
  ( addSteps,
  )
where

import Data.Char (ord)
import Data.List.Split (splitOn)

hash :: Char -> Integer -> Integer
hash = ((((`mod` 256) . (* 17)) .) . (+)) . (toInteger . ord)

addSteps :: IO ()
addSteps =
  do
    input <- readFile "input.txt"
    print $ sum $ map (foldl (flip hash) 0) $ splitOn "," $ filter (/= '\n') input
