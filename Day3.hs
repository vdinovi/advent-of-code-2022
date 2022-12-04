import System.IO
import System.Environment

import Data.Char (ord)
import Data.List (intersect, nub)
import Data.Ix (inRange)

priority :: Char -> Int
priority ch = priority' $ ord ch 

priority' :: Int -> Int
priority' o 
  | inRange (97, 122) o = (o - 96)
  | inRange (65, 90) o  = (o - 64 + 26)

-- assumes string is even length
split :: [Char] -> ([Char], [Char])
split str = splitAt (div (length str) 2) str

parse :: [Char] -> Int
parse line = sum . map priority $ dups
  where
    xs = split line
    dups = nub (intersect (fst xs) (snd xs))

part1 :: [Char] -> Int
part1 input = sum . map parse . lines $ input

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents
 
