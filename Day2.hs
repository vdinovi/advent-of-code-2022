import System.IO
import System.Environment

data Play = Rock | Paper | Scissors deriving (Eq, Show)

decode :: Char -> Char
decode 'X' = 'A'
decode 'Y' = 'B'
decode 'Z' = 'C'

play :: Char -> Play
play 'A' = Rock
play 'B' = Paper
play 'C' = Scissors

beats :: Play -> Play -> Bool
beats x y = y == beats' x

beats' :: Play -> Play
beats' Rock     = Scissors
beats' Paper    = Rock
beats' Scissors = Paper

value :: (Num a) => Play -> a
value Rock     = 1
value Paper    = 2
value Scissors = 3

score :: (Num a) => (Play, Play) -> a
score (x, y) = score' (x, y) + value y

score' :: (Num a) => (Play, Play) -> a
score' (x, y)
  | y `beats` x = 6
  | y == x      = 3
  | otherwise   = 0

translate :: [Char] -> (Play, Play)
translate x = 
  ( play . head $ x
  , play . decode . last $ x)

parse :: ((Play, Play) -> b) -> [Char] -> [b]
parse f = map (f . translate) . lines


part1 :: (Num a) => [Char] -> a
part1 input = sum . parse score $ input

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents
