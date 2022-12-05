module Lib.Char (trim) where

import Data.Char (isSpace)

trim :: [Char] -> [Char]
trim = f . f
  where f = reverse . dropWhile isSpace

