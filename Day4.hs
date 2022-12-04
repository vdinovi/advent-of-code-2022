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
 
within :: (Ord a) => (a,a) -> (a,a) -> Bool
within (a,b) (x,y) = a >= x && b <= y

parse :: [Char] -> [[(Int,Int)]]
parse str = map parsePair . lines $ str
  where parseRange = map read . split (=='-')
        parsePair = (map pair . map parseRange . split (==','))

part1 :: [Char] -> Int
part1 str = length . filter contains . parse $ str
  where contains [x,y] = x `within` y || y `within` x

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents

