module Main where

import Data.Set (Set, empty, insert, lookupGT, lookupLE, toList)
import Data.Tree (flatten)

data Seed = Seed {seed, seedLen :: Integer} deriving (Show)

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

-- Computes the number of id's you'd need to increment `id` by before it would
-- not map to the same range of the dst. `Nothing` means id can increment by an
-- infinite amount in the same range in dst.
distToDiff :: Integer -> IdMap -> Maybe Integer
distToDiff id (IdMap set) =
  let resLE = lookupLE IdRange {src = id, dst = 0, len = 0} set
      resGT = lookupGT IdRange {src = id, dst = 0, len = 0} set
   in case (resLE, resGT) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just IdRange {src = src}) -> Just (src - id)
        (Just IdRange {src = src, len = len}, Nothing) ->
          if id >= src + len
            then Nothing
            else Just (src + len - id)
        (Just IdRange {src = src, len = len}, Just IdRange {src = src2}) ->
          if id >= src + len
            then Just (src2 - id)
            else Just (src + len - id)

minMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
minMaybe Nothing Nothing = Nothing
minMaybe Nothing b = b
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just (min a b)

insertId :: IdRange -> IdMap -> IdMap
insertId idRange (IdMap set) = IdMap (insert idRange set)

seedToLocation :: Integer -> [IdMap] -> Integer
seedToLocation = foldl lookupId

seedToLocations :: Integer -> [IdMap] -> [Integer]
seedToLocations id [] = [id]
seedToLocations id (idMap : idMaps) =
  let mappedId = lookupId id idMap
   in (id : seedToLocations mappedId idMaps)

seedToLocationWithDiff :: Integer -> [IdMap] -> (Integer, Maybe Integer)
seedToLocationWithDiff id [] = (id, Nothing)
seedToLocationWithDiff id (idMap : idMaps) =
  let mappedId = lookupId id idMap
      diff = distToDiff id idMap
      (finalId, remDiff) = seedToLocationWithDiff mappedId idMaps
   in (finalId, minMaybe diff remDiff)

minSeedRange :: Seed -> [IdMap] -> Integer
minSeedRange (Seed {seed = seed, seedLen = len}) idMaps =
  let (location, diff) = seedToLocationWithDiff seed idMaps
   in case diff of
        Just diff ->
          if diff >= len
            then location
            else min location (minSeedRange (Seed {seed = seed + diff, seedLen = len - diff}) idMaps)
        Nothing -> location

parseSeeds :: [String] -> [Integer]
parseSeeds [] = []
parseSeeds ("seeds:" : rem) = parseSeeds rem
parseSeeds (seed : rem) = read seed : parseSeeds rem

parseSeeds2 :: [String] -> [Seed]
parseSeeds2 [] = []
parseSeeds2 ("seeds:" : rem) = parseSeeds2 rem
parseSeeds2 (seed : len : rem) = Seed {seed = read seed, seedLen = read len} : parseSeeds2 rem

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

parseContents2 :: String -> ([Seed], [IdMap])
parseContents2 contents =
  let (seeds : "" : rem) = lines contents
   in (parseSeeds2 (words seeds), parseMaps rem)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  -- let (seeds, idMaps) = parseContents contents
  --  in print (minimum (map (`seedToLocation` idMaps) seeds))
  let (seeds, idMaps) = parseContents2 contents
   in print (minimum (map (`minSeedRange` idMaps) seeds))
