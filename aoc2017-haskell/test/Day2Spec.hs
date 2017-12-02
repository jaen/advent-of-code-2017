module Day2Spec (
  extremaDifferenceChecksum,
  evenlyDivisibleChecksum
) where

import Test.Tasty
import Test.Tasty.HUnit

import Day2

example1 =
  "5 1 9 5\n\
  \7 5 3\n\
  \2 4 6 8"

example2 =
  "5 9 2 8\n\
  \9 4 7 3\n\
  \3 8 6 5"

extremaDifferenceChecksum :: TestTree
extremaDifferenceChecksum =
    testGroup "Testing checksum for extremaDifference" $ [
      testCase "checksum = 18" $ 18 @?= (subject example1)
    ]
  where
    subject =
      checksum extremaDifference . parseInput

evenlyDivisibleChecksum :: TestTree
evenlyDivisibleChecksum =
    testGroup "Testing checksum for evenlyDivisible" $ [
      testCase "checksum = 9" $ 9 @?= (subject example2)
    ]
  where
    subject =
      checksum evenlyDivisible . parseInput
