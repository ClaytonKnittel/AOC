module Main where

import Data.Set (Set, empty, insert, lookupLE, toList)

data IdRange = IdRange {src, dst, len :: Integer}

instance Eq IdRange where
  IdRange {src = src1} == IdRange {src = src2} = src1 == src2

instance Ord IdRange where
  compare (IdRange {src = src1}) (IdRange {src = src2}) = src1 `compare` src2

instance Show IdRange where
  show IdRange {src = src, dst = dst, len = len} = show src ++ " " ++ show dst ++ " " ++ show len

newtype IdMap = IdMap (Set IdRange)

instance Show IdMap where
  show (IdMap set) = show (toList set)

lookupId :: Integer -> IdMap -> Integer
lookupId id (IdMap set) =
  let res = lookupLE IdRange {src = id, dst = 0, len = 0} set
   in case res of
        Just IdRange {src = src, dst = dst, len = len} ->
          if id >= src && id < src + len
            then dst + id - src
            else id
        Nothing -> id

insertId :: IdRange -> IdMap -> IdMap
insertId idRange (IdMap set) = IdMap (insert idRange set)

seedToLocation :: Integer -> [IdMap] -> Integer
seedToLocation = foldl lookupId

seedToLocations :: Integer -> [IdMap] -> [Integer]
seedToLocations id [] = [id]
seedToLocations id (idMap : idMaps) =
  let mappedId = lookupId id idMap
   in (id : seedToLocations mappedId idMaps)

parseSeeds :: [String] -> [Integer]
parseSeeds [] = []
parseSeeds ("seeds:" : rem) = parseSeeds rem
parseSeeds (seed : rem) = read seed : parseSeeds rem

parseIdRanges :: [String] -> [IdMap]
parseIdRanges [] = [IdMap empty]
parseIdRanges ("" : rem) = IdMap empty : parseMaps rem
parseIdRanges (idRange : rem) =
  case words idRange of
    [dst, src, len] ->
      let (idMap : idMaps) = parseIdRanges rem
       in (insertId IdRange {src = read src, dst = read dst, len = read len} idMap : idMaps)

parseMaps :: [String] -> [IdMap]
parseMaps (mapName : rem) = case words mapName of
  [_, "map:"] -> parseIdRanges rem

parseContents :: String -> ([Integer], [IdMap])
parseContents contents =
  let (seeds : "" : rem) = lines contents
   in (parseSeeds (words seeds), parseMaps rem)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (seeds, idMaps) = parseContents contents
   in print (minimum (map (`seedToLocation` idMaps) seeds))
