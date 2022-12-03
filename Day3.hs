import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print contents
