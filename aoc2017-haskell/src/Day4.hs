module Day4 (
  isValidPassphrase,
  isValidAnagramPassphrase,
  parseInput
) where

import qualified Data.Set as Set
import Data.List (sort)

unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

isValidPassphrase :: String -> Bool
isValidPassphrase line = lengthsMatch
  where
    wordsInLine = words line
    uniqueWords = unique wordsInLine
    lengthsMatch = length wordsInLine == length uniqueWords

isValidAnagramPassphrase :: String -> Bool
isValidAnagramPassphrase line = lengthsMatch
  where
    -- Pretending to be smart https://stackoverflow.com/questions/29370955/are-two-strings-anagrams-or-not
    canonicalWordsInLine = map sort $ words line
    uniqueWords = unique canonicalWordsInLine
    lengthsMatch = length canonicalWordsInLine == length uniqueWords

parseInput :: String -> [String]
parseInput = lines
