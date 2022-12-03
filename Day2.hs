import System.IO
import System.Environment

data Play = Rock | Paper | Scissors deriving (Eq, Show)

data Result = Win | Tie | Lose deriving (Eq, Show)

play :: Char -> Play
play 'A' = Rock
play 'B' = Paper
play 'C' = Scissors

decode :: Char -> Char
decode 'X' = 'A'
decode 'Y' = 'B'
decode 'Z' = 'C'

result :: Char -> Result
result 'X' = Lose
result 'Y' = Tie
result 'Z' = Win

beats :: Play -> Play -> Bool
beats x y = y == beats' x

beats' :: Play -> Play
beats' Rock     = Scissors
beats' Paper    = Rock
beats' Scissors = Paper

-- loses :: Play -> Play -> Bool
-- loses = beats . flip

loses' :: Play -> Play
loses' Rock     = Paper
loses' Paper    = Scissors
loses' Scissors = Rock

deduce :: Result -> Play -> Play
deduce Win p  = loses' p
deduce Tie p  = p
deduce Lose p = beats' p

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

interpret :: [Char] -> (Play, Play)
interpret xs = (p1, p2)
  where
    p1 = play . head $ xs
    p2 = play . decode . last $ xs

interpret' :: [Char] -> (Play, Play)
interpret' xs = (p1, p2)
  where
    p1 = play . head $ xs
    r  = result . last $ xs
    p2 = deduce r p1

parse :: ([Char] -> b) -> [Char] -> [b]
parse f = map f . lines

part1 :: (Num a) => [Char] -> a
part1 input = sum . map score . parse interpret $ input

part2 :: (Num a) => [Char] -> a
part2 input = sum . map score . parse interpret' $ input

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents
  print . part2 $ contents
