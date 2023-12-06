module Main where

import Data.Char (isDigit)
import Data.Map (Map, empty, findWithDefault, insert, insertWith, keys, member)

-- Split a string at every occurrence of a character.
splitString :: String -> Char -> [String]
splitString "" _ = [""]
splitString (s : ss) c
  | s == c = "" : splitString ss c
  | otherwise = (s : head res) : tail res
  where
    res = splitString ss c

-- Parses a string into a sequence of ("<string val>", start index) pairs.
parseString' :: String -> Integer -> [(String, Integer)]
parseString' "" _ = []
parseString' (s : ss) idx
  | isDigit s = case parseString' ss (idx + 1) of
    [] -> [([s], idx)]
    res@((s2, idx2) : rem)
      | isDigit (head s2) && (idx + 1 == idx2) ->
        (s : s2, idx) : rem
      | otherwise -> ([s], idx) : res
  | s == '.' = parseString' ss (idx + 1)
  | otherwise = ([s], idx) : parseString' ss (idx + 1)

parseString :: String -> [(String, Integer)]
parseString s = parseString' s 0

data Point = Point {x, y :: Integer} deriving (Eq, Ord)

instance Show Point where
  show (Point {x = x, y = y}) = "(" ++ show x ++ "," ++ show y ++ ")"

addPoint :: Point -> Point -> Point
addPoint (Point {x = x1, y = y1}) (Point {x = x2, y = y2}) = Point {x = x1 + x2, y = y1 + y2}

data SymType = PartNum Integer Integer | PartSym Char

instance Show SymType where
  show (PartNum x len) = show x ++ "(" ++ show len ++ ")"
  show (PartSym c) = show c

instance Eq SymType where
  (PartNum x len) == (PartNum y len2) = x == y
  (PartNum x len) == (PartSym y) = False
  (PartSym x) == (PartNum y len) = False
  (PartSym x) == (PartSym y) = True

instance Ord SymType where
  compare (PartNum x len) (PartNum y len2) = compare x y
  compare (PartNum x len) (PartSym y) = LT
  compare (PartSym x) (PartNum y len) = GT
  compare (PartSym x) (PartSym y) = EQ

symPerimeter :: SymType -> [Point]
symPerimeter (PartSym c) = symPerimeter (PartNum 0 1)
symPerimeter (PartNum _ len) = [Point {x = x, y = y} | y <- [-1 .. 1], x <- [-1 .. len], y /= 0 || x == -1 || x == len]

data Sym = Sym {t :: SymType, p :: Point} deriving (Eq, Ord)

instance Show Sym where
  show (Sym {t = t, p = p}) = show t ++ "@" ++ show p

makeSym :: String -> Integer -> Integer -> Sym
makeSym s x y
  | isDigit (head s) = Sym {t = PartNum (read s :: Integer) (toInteger (length s)), p = Point {x = x, y = y}}
  | otherwise = Sym {t = PartSym (head s), p = Point {x = x, y = y}}

symIsNum :: Sym -> Bool
symIsNum (Sym {t = (PartNum _ _)}) = True
symIsNum (Sym {t = (PartSym _)}) = False

perimeter :: Sym -> [Point]
perimeter (Sym {t = t, p = p}) = map (addPoint p) (symPerimeter t)

symIsStar :: Sym -> Bool
symIsStar (Sym {t = (PartSym sym)}) = sym == '*'

symIsPart :: Sym -> SymMap -> Bool
symIsPart sym symMap = any (`member` getMap symMap) (perimeter sym)

newtype SymMap = SymMap (Map Point Sym)

getMap :: SymMap -> Map Point Sym
getMap (SymMap map) = map

mkSymMap :: [Sym] -> SymMap
mkSymMap [] = SymMap empty
mkSymMap (sym@(Sym {t = _, p = p}) : syms) = SymMap (insert p sym (getMap (mkSymMap syms)))

newtype NumMap = NumMap (Map Integer [Sym])

getNumMap :: NumMap -> Map Integer [Sym]
getNumMap (NumMap map) = map

getNums :: NumMap -> Integer -> [Sym]
getNums (NumMap map) row = findWithDefault [] row map

mkNumMap :: [Sym] -> NumMap
mkNumMap [] = NumMap empty
mkNumMap (sym@(Sym {t = _, p = Point {x = _, y = y}}) : syms) = NumMap (insertWith (++) y [sym] (getNumMap (mkNumMap syms)))

gearNeighbors :: Sym -> NumMap -> [Sym]
gearNeighbors Sym {t = _, p = Point {x = x, y = y}} nums = filter (\Sym {t = PartNum _ len, p = Point {x = sx, y = _}} -> sx <= x + 1 && sx + len >= x) (getNums nums (y - 1) ++ getNums nums y ++ getNums nums (y + 1))

parseInput :: String -> [Sym]
parseInput input = concat (zipWith (\line y -> map (\(sym, x) -> makeSym sym x y) (parseString line)) (lines input) [0 ..])

main :: IO ()
main = do
  content <- readFile "input.txt"
  let syms = parseInput content
  let parts = filter (not . symIsNum) syms
  let symMap = mkSymMap parts
  let nums = filter symIsNum syms
  let numsMap = mkNumMap nums
  -- print (sum (map (\Sym {t = PartNum x _} -> x) (filter (`symIsPart` symMap) nums)))
  print (sum (map (\[Sym {t = PartNum val1 _}, Sym {t = PartNum val2 _}] -> val1 * val2) (filter (\x -> length x == 2) (map (`gearNeighbors` numsMap) (filter symIsStar parts)))))

-- print (filter (\x -> length x == 2) (map (`gearNeighbors` numsMap) (filter symIsStar parts)))

-- print (map (symPerimeter . \Sym { t=t } -> t) nums)
-- print (map perimeter nums)
-- print (getMap symMap)
