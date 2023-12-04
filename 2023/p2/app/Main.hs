module Main where

-- Split a string at the first occurrence of a character, returning the string after that character.
splitStringOnce :: String -> Char -> (String, String)
splitStringOnce "" _ = ("", "")
splitStringOnce (s:ss) c
  | s == c    = ("", ss)
  | otherwise = (s : pre, post)
    where (pre, post) = splitStringOnce ss c

-- Split a string at every occurrence of a character.
splitString :: String -> Char -> [String]
splitString "" _ = [""]
splitString (s:ss) c
  | s == c    = "" : splitString ss c
  | otherwise = (s : head res):tail res
    where res = splitString ss c

data Game = Game { red, green, blue :: Integer }
instance Show Game where
  show (Game { red = r, green = g, blue = b }) = "r:" ++ show r ++ ", g:" ++ show g ++ ", b:" ++ show b

gameMax :: Game -> Game -> Game
gameMax (Game { red = r1, green = g1, blue = b1 }) (Game { red = r2, green = g2, blue = b2 }) = Game { red = max r1 r2, green = max g1 g2, blue = max b1 b2 }

gamePower :: Game -> Integer
gamePower (Game { red = r, green = g, blue = b }) = r * g * b

parseGame :: String -> Game
parseGame "" = Game { red = 0, green = 0, blue = 0 }
parseGame (' ':rem) =
  let (countStr, rem2) = splitStringOnce rem ' '
      (color, rem3) = splitStringOnce rem2 ','
      count = read countStr :: Integer
      game = parseGame rem3
  in case color of
       "red" -> game { red = count }
       "green" -> game { green = count }
       "blue" -> game { blue = count }

parseGames :: String -> [Game]
parseGames line = map parseGame (splitString line ';')

parseGameLine :: String -> ([Game], Integer)
parseGameLine ('G':'a':'m':'e':' ':rem) = (parseGames gameLine, read ident :: Integer)
  where (ident, gameLine) = splitStringOnce rem ':'

validGame :: Game -> Bool
validGame (Game { red = r, green = g, blue = b }) = r <= 12 && g <= 13 && b <= 14

-- isPossible :: String -> Integer
-- isPossible s = if all validGame games then ident else 0
--   where (games, ident) = parseGameLine s

fewestCubesPower :: String -> Integer
fewestCubesPower s = gamePower (foldr gameMax (Game { red = 0, green = 0, blue = 0 }) games)
  where (games, _) = parseGameLine s

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  -- print (foldr ((+) . isPossible) 0 (lines contents))
  print (foldr ((+) . fewestCubesPower) 0 (lines contents))
