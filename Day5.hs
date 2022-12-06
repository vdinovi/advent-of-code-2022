import System.IO (readFile)
import System.Environment (getArgs)

import Text.Read (readMaybe)
import Data.List (transpose)
import Data.Char (isSpace, isNumber)
import qualified Data.Map as Map

import Lib.Char (trim)
import Lib.List (splitEvery, splitOn, tailGuard, headGuard)

import System.IO.Unsafe (unsafePerformIO)

type Stack = [Char]

type Crate = Char

data Instruction 
  = Move Int Int Int
  deriving(Eq, Show)

data Token 
  = TSpace
  | TCrate Char
  | TNum Int
  | TWord String
  | TUnknown String
  deriving (Eq, Show)

data Section 
  = Data [[Token]]
  | Text [[Token]]
  deriving(Eq, Show)

tokenizeData :: [String] -> Section
tokenizeData ss = Data (map (map tokenizeData' . map (take 3) . splitEvery 4) ss)

tokenizeData' :: [Char] -> Token
tokenizeData' (x:y:z:_)
  | all isSpace [x,y,z]  = TSpace
  | x == '[' && z == ']' = TCrate y
  | isNumber y           = TNum (read [y])
  | otherwise            = error $ "unexpected string '" ++ [x,y,z] 

tokenizeText :: [String] -> Section
tokenizeText ss = Text (map (map tokenizeText'. words) ss)

tokenizeText' :: [Char] -> Token
tokenizeText' cs =
  case readMaybe cs :: Maybe Int of
    Just n -> TNum n
    Nothing -> TWord cs

parseData :: Section -> [Stack] 
parseData (Data d) = snd . foldl buildStacks (m,s) $ tail ts'
  where
    ts = map (takeWhile (not . (==)TSpace) . reverse) $ transpose d
    n = maximum . map (\(TNum t) -> t) . map head $ ts
    s = take n . repeat $ ""
    ts' = concat ts
    (TNum m) = head ts'
parseData _ = error $ "Invalid data section"

buildStacks :: (Int,[Stack]) -> Token -> (Int,[Stack])
buildStacks (_,ss) (TNum n') = (n',ss)
buildStacks (n,ss) (TCrate c) = (n,ss')
  where (xs,ys) = splitAt n ss
        ss' = xs ++ [c : headGuard ys] ++ tailGuard ys

parseInstructions :: Section -> [Instruction]
parseInstructions (Text t) = map parseInstruction t
parseInstructions _ = error $ "Invalid text section"

parseInstruction :: [Token] -> Instruction
parseInstruction ((TWord "move"):ts) = Move count from to
  where (TNum count:TWord "from":TNum from:TWord "to":TNum to:_) = ts
parseInstruction ((TWord op):ts) = error $ "invalid instruction " ++ op
parseInstruction _ = error $ "invalid instruction"

update :: ([Char] -> [Char]) -> [Stack] -> Instruction -> [Stack]
update order stacks (Move count from to) = map (update' (from,a) (to,b)) . zip [0..] $ stacks
  where a = drop count $ stacks !! from
        b = (order . take count $ stacks !! from) ++ (stacks !! to)

update' :: (Int,Stack) -> (Int,Stack) -> (Int,Stack) -> Stack
update' (i,x) (j,y) (k,z)
  | k == i = x
  | k == j = y
  | otherwise = z

part1 :: [Char] -> [Char]
part1 str = map head . filter (not . null) $ stacks'
  where (d,t) = span (not . null) . lines $ str
        stacks = parseData . tokenizeData $ d
        instrs = parseInstructions . tokenizeText $ tail t
        stacks' = foldl (\acc instr -> update reverse acc instr) stacks instrs

part2 :: [Char] -> [Char]
part2 str = map head . filter (not . null) $ stacks'
  where (d,t) = span (not . null) . lines $ str
        stacks = parseData . tokenizeData $ d
        instrs = parseInstructions . tokenizeText $ tail t
        stacks' = foldl (\acc instr -> update id acc instr) stacks instrs

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile . head $ args
  print . part1 $ contents
  print . part2 $ contents

