module Day2 (
  checksum,
  extremaDifference,
  evenlyDivisible,
  parseInput
) where

import Data.Char (isSpace)
import Data.List (groupBy)

checksum :: (Num a, Ord a) => ([a] -> a) -> [[a]] -> a
checksum lineValue spreadsheet =
  sum $ map lineValue spreadsheet

extremaDifference :: (Num a, Ord a) => [a] -> a
extremaDifference line = maximum line - minimum line

evenlyDivisible :: (Integral a, Ord a) => [a] -> a
evenlyDivisible line =
  head [x `div` y | x <- line,
                    y <- line,
                    x `rem` y == 0,
                    x /= y]

--

parseInput :: String -> [[Int]]
parseInput str = map parseLine $ lines str
  where
    parseLine                 = map read . splitByWhitespace
    splitByWhitespace         = filter (all $ not . isSpace) .
                                groupBy consecutiveWhitespace
    consecutiveWhitespace l r = not (isSpace l || isSpace r)
