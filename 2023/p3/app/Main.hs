module Main where
import Data.Char (isDigit)
import Data.Map (Map, empty, insert)

-- Split a string at every occurrence of a character.
splitString :: String -> Char -> [String]
splitString "" _ = [""]
splitString (s:ss) c
  | s == c    = "" : splitString ss c
  | otherwise = (s : head res):tail res
    where res = splitString ss c

-- Parses a string into a sequence of ("<string val>", start index) pairs.
parseString' :: String -> Integer -> [(String, Integer)]
parseString' "" _ = []
parseString' (s:ss) idx
  | isDigit s = case parseString' ss (idx + 1) of
                  []                    -> [([s], idx)]
                  res@((s2, idx2):rem)
                    | isDigit (head s2) && (idx + 1 == idx2)
                                        -> (s:s2,idx) : rem
                    | otherwise         -> ([s], idx) : res
  | s == '.'  = parseString' ss (idx + 1)
  | otherwise = ([s], idx) : parseString' ss (idx + 1)

parseString :: String -> [(String, Integer)]
parseString s = parseString' s 0

data Point = Point { x, y :: Integer } deriving (Eq, Ord)
instance Show Point where
  show (Point { x = x, y = y }) = "(" ++ show x ++ "," ++ show y ++ ")"

addPoint :: Point -> Point -> Point
addPoint (Point { x = x1, y = y1 }) (Point { x = x2, y = y2 }) = Point { x = x1+x2, y = y1+y2 }

data SymType = PartNum Integer Integer | PartSym Char
instance Show SymType where
  show (PartNum x len) = show x
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
symPerimeter (PartSym c)     = symPerimeter (PartNum 0 1)
symPerimeter (PartNum _ len) = [Point { x=x, y=y } | x <- [-1..1], y <- [-1..len], x /= 0 || y == -1 || y == len]

data Sym = Sym { t :: SymType, p :: Point } deriving (Eq, Ord)
instance Show Sym where
  show (Sym { t = t, p = p }) = show t ++ "@" ++ show p

makeSym :: String -> Integer -> Integer -> Sym
makeSym s x y
  | isDigit (head s) = Sym { t = PartNum (read s :: Integer) (toInteger (length s)), p = Point { x = x, y = y } }
  | otherwise        = Sym { t = PartSym (head s), p = Point { x = x, y = y } }

symIsNum :: Sym -> Bool
symIsNum (Sym { t = (PartNum _ _) }) = True
symIsNum (Sym { t = (PartSym _) }) = False

perimeter :: Sym -> [Point]
perimeter (Sym { t=t, p=p }) = map (addPoint p) (symPerimeter t)

newtype SymMap = SymMap (Map Point Sym)
getMap :: SymMap -> Map Point Sym
getMap (SymMap map) = map

mkSymMap :: [Sym] -> SymMap
mkSymMap []         = SymMap empty
mkSymMap (sym@(Sym { t = _, p = p }):syms) = SymMap (insert p sym (getMap (mkSymMap syms)))

parseInput :: String -> [Sym]
parseInput input = concat (zipWith (\ line y -> map (\ (sym, x) -> makeSym sym x y) (parseString line)) (lines input) [0..])

data NumPos = NumPos { pos :: Point, val, len :: Integer }

main :: IO ()
main = do
  content <- readFile "input.txt"
  print (parseInput content)
