module Lib.List (splitEvery, splitOn, tailGuard, headGuard) where

-- Splits list into sublists of length n
-- assumes list is divisible by n
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

-- Splits list into sublists based on boolean predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p xs =
  case dropWhile p xs of
    [] -> []
    xs' -> w : splitOn p xs''
      where (w, xs'') = break p xs'

-- Guard tail against empty lists
tailGuard xs
  | null xs = []
  | otherwise = tail xs

-- Guard head against empty lists
headGuard xs
  | null xs = []
  | otherwise = head xs


