import System.IO (readFile)
import System.Environment (getArgs)

import Data.List (nub)

search :: (Eq a) => Int -> [a] -> [a] -> Maybe Int
search _ [] _ = Nothing
search _ window [] = Nothing
search n window (x:xs)
  | (==) 5 . length . nub $ x:window = Just n 
  | otherwise = search (n+1) (x:init window) xs

part1 :: [Char] -> Int
part1 str = n 
  where (window,rest) = splitAt 4 str
        n = case search 4 (reverse window) rest of
          Just n -> n
          Nothing -> error "No signal, just noise"

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents
