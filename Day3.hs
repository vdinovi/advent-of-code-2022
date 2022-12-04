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

-- assumes list is is divisible by n
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

-- assumes list is divisible by 2
divide :: [a] -> [[a]]
divide xs = splitEvery (div (length xs) 2) xs

part1 :: [Char] -> Int
part1 input = sum . map part1' . lines $ input

part1' :: [Char] -> Int
part1' line = sum . map priority . nub . foldr1 intersect $ divide line

part2 :: [Char] -> Int
part2 input = sum . map part2'. splitEvery 3 . lines $ input

part2' :: [[Char]] -> Int
part2' lines = sum . map priority . nub . foldr1 intersect $ lines

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents
  print . part2 $ contents

