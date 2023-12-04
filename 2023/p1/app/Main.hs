module Main where
import GHC.IO.FD (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hClose)
import Data.Char (isDigit, digitToInt)

tokenize :: String -> [Integer]
tokenize ""                        = []
tokenize ('z':'e':'r':'o':rem)     = 0:tokenize rem
tokenize ('o':'n':'e':rem)         = 1:tokenize rem
tokenize ('t':'w':'o':rem)         = 2:tokenize rem
tokenize ('t':'h':'r':'e':'e':rem) = 3:tokenize rem
tokenize ('f':'o':'u':'r':rem)     = 4:tokenize rem
tokenize ('f':'i':'v':'e':rem)     = 5:tokenize rem
tokenize ('s':'i':'x':rem)         = 6:tokenize rem
tokenize ('s':'e':'v':'e':'n':rem) = 7:tokenize rem
tokenize ('e':'i':'g':'h':'t':rem) = 8:tokenize rem
tokenize ('n':'i':'n':'e':rem)     = 9:tokenize rem
tokenize ('0':rem)                 = 0:tokenize rem
tokenize ('1':rem)                 = 1:tokenize rem
tokenize ('2':rem)                 = 2:tokenize rem
tokenize ('3':rem)                 = 3:tokenize rem
tokenize ('4':rem)                 = 4:tokenize rem
tokenize ('5':rem)                 = 5:tokenize rem
tokenize ('6':rem)                 = 6:tokenize rem
tokenize ('7':rem)                 = 7:tokenize rem
tokenize ('8':rem)                 = 8:tokenize rem
tokenize ('9':rem)                 = 9:tokenize rem
tokenize (_:rem)                   = tokenize rem

rtokenize :: String -> [Integer]
rtokenize ""                        = []
rtokenize ('o':'r':'e':'z':rem)     = 0:rtokenize rem
rtokenize ('e':'n':'o':rem)         = 1:rtokenize rem
rtokenize ('o':'w':'t':rem)         = 2:rtokenize rem
rtokenize ('e':'e':'r':'h':'t':rem) = 3:rtokenize rem
rtokenize ('r':'u':'o':'f':rem)     = 4:rtokenize rem
rtokenize ('e':'v':'i':'f':rem)     = 5:rtokenize rem
rtokenize ('x':'i':'s':rem)         = 6:rtokenize rem
rtokenize ('n':'e':'v':'e':'s':rem) = 7:rtokenize rem
rtokenize ('t':'h':'g':'i':'e':rem) = 8:rtokenize rem
rtokenize ('e':'n':'i':'n':rem)     = 9:rtokenize rem
rtokenize ('0':rem)                 = 0:rtokenize rem
rtokenize ('1':rem)                 = 1:rtokenize rem
rtokenize ('2':rem)                 = 2:rtokenize rem
rtokenize ('3':rem)                 = 3:rtokenize rem
rtokenize ('4':rem)                 = 4:rtokenize rem
rtokenize ('5':rem)                 = 5:rtokenize rem
rtokenize ('6':rem)                 = 6:rtokenize rem
rtokenize ('7':rem)                 = 7:rtokenize rem
rtokenize ('8':rem)                 = 8:rtokenize rem
rtokenize ('9':rem)                 = 9:rtokenize rem
rtokenize (_:rem)                   = rtokenize rem

score :: String -> Integer
-- score line = 10 * head digits + last digits
--   where digits = map (toInteger . digitToInt) (filter isDigit line)
score line = 10 * head (tokenize line) + head (rtokenize (reverse line))

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  print (foldr (\line total -> score line + total) 0 (lines contents))
