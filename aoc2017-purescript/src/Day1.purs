module Day1 (
  sumOfPairwiseEqual,
  parseInput
) where

import Prelude

import Data.Char.Unicode (digitToInt)
import Data.Foldable (sum, foldM)
import Data.List.Lazy (List, zip, drop, cycle, filter)
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Semiring (class Semiring)
import Data.String (toCharArray)
import Data.Tuple (Tuple(..), fst)
import Data.Array (toUnfoldable, snoc)
import Data.Unfoldable (class Unfoldable)
import Data.Monoid (append, (<>))
import Data.Functor (map)
import Data.Traversable (sequence)

sumOfPairwiseEqual :: forall a . Semiring a => Eq a => Int -> List a -> a
sumOfPairwiseEqual step list =
  let paired = zip list $ drop step $ cycle list
      equal  = filter (\(Tuple r l) -> l == r) $ paired
  in
    sum $ map fst equal

--

parseInput :: forall a . Unfoldable a => String -> a Int
parseInput =
      toCharArray
  >>> map digitToInt
  -- >>> foldM (\acc x -> Just $ x : acc) []
  >>> sequence
  >>> fromMaybe []
  >>> toUnfoldable
