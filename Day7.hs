import System.IO (readFile)
import System.Environment (getArgs)

import Data.List (nub)

search :: (Eq a) => Int -> [a] -> [a] -> Maybe Int
search _ [] _ = Nothing
search _ window [] = Nothing
search n window (x:xs)
  | (<) (length window) . length . nub $ x:window = Just n
  | otherwise = search (n+1) (x:init window) xs

solve :: Int -> [Char] -> Int
solve len str = n
  where (window,rest) = splitAt len str
        n = case search len (reverse window) rest of
          Just n -> n
          Nothing -> error "No signal, just noise"

part1 :: [Char] -> Int
part1 str = solve 4 str

part2 :: [Char] -> Int
part2 str = (solve 13 str) + 1

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents
  print . part2 $ contents

