import System.IO (readFile)
import System.Environment (getArgs)

split :: (Char -> Bool) -> String -> [String]
split p s =
  case dropWhile p s of
    "" -> []
    s' -> w : split p s''
      where (w, s'') = break p s'

pair :: [a] -> (a,a)
pair [x,y] = (x,y)
 
covers :: (Ord a) => (a,a) -> (a,a) -> Bool
covers (a,b) (x,y) = a >= x && b <= y

disjoint :: (Ord a) => (a,a) -> (a,a) -> Bool
disjoint (a,b) (x,y) = (a < x && b < x) || (a > y && b > y)

parse :: [Char] -> [[(Int,Int)]]
parse str = map parsePair . lines $ str
  where parseRange = map read . split (=='-')
        parsePair = (map pair . map parseRange . split (==','))

part1 :: [Char] -> Int
part1 str = length . filter contains . parse $ str
  where contains [x,y] = x `covers` y || y `covers` x

part2 :: [Char] -> Int
part2 str = length . filter overlaps . parse $ str
  where overlaps [x,y] = not (x `disjoint` y)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents
  print . part2 $ contents
