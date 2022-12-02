import System.IO
import System.Environment
import Data.List

-- Splits list into sublists by null delimiters
splitByNull :: (Foldable t) => [t a] -> [[t a]]
splitByNull [] = []
splitByNull xs = next:rest where
    split = span (not . null) $ xs
    next = fst split
    rest = 
        if null . snd $ split 
            then []
            else  splitByNull . tail . snd $ split

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile . head $ args
    let buckets = splitByNull . lines $ contents
        totals = [sum . map (read :: [Char] -> Int) $ x | x <- buckets]
        topThree = take 3 . sortBy (flip compare) $ totals
    print . sum $ topThree
