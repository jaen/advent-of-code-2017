module Day1 (
  sumOfPairwiseEqual,
  parseInput
) where

import Data.Char (digitToInt)

sumOfPairwiseEqual :: (Num a, Eq a) => Int -> [a] -> a
sumOfPairwiseEqual step list =
  let
    paired = zip list $ drop step $ cycle list
    equal  = filter (\(l, r) -> l == r) $ paired
  in
    sum $ map fst equal

--

parseInput :: String -> [Int]
parseInput = map digitToInt
