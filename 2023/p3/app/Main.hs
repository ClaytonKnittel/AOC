module Main where
import Data.Char (isDigit)

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

data SymType = PartNum Integer | PartSym Char
instance Show SymType where
  show (PartNum x) = show x
  show (PartSym c) = show c
data Point = Point { x, y :: Integer }
instance Show Point where
  show (Point { x = x, y = y }) = "(" ++ show x ++ "," ++ show y ++ ")"
data Sym = Sym { t :: SymType, p :: Point }
instance Show Sym where
  show (Sym { t = t, p = p }) = show t ++ "@" ++ show p

makeSym :: String -> Integer -> Integer -> Sym
makeSym s x y
  | isDigit (head s) = Sym { t = PartNum (read s :: Integer), p = Point { x = x, y = y } }
  | otherwise        = Sym { t = PartSym (head s), p = Point { x = x, y = y } }

parseInput :: String -> [Sym]
parseInput input = concat (zipWith (\ line y -> map (\ (sym, x) -> makeSym sym x y) (parseString line)) (lines input) [0..])

data NumPos = NumPos { pos :: Point, val, len :: Integer }

main :: IO ()
main = do
  content <- readFile "input.txt"
  print (parseInput content)
