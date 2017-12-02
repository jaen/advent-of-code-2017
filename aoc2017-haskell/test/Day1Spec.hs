module Day1Spec (
  nextDigit,
  halfwayListAwayDigit
) where

import Test.Tasty
import Test.Tasty.HUnit

import Day1

nextDigit :: TestTree
nextDigit =
    testGroup "Testing sumOfPairwiseEqual for next digit" $ [
      testCase "1122 = 3"     $ 3 @?= (subject "1122"),
      testCase "1111 = 4"     $ 4 @?= (subject "1111"),
      testCase "1234 = 0"     $ 0 @?= (subject "1234"),
      testCase "91212129 = 9" $ 9 @?= (subject "91212129")
    ]
  where
    subject =
      sumOfPairwiseEqual 1 . parseInput

halfwayListAwayDigit :: TestTree
halfwayListAwayDigit =
    testGroup "Testing sumOfPairwiseEqual for halfway list away digit" $ [
      testCase "1212 = 6"      $  6 @?= (subject "1212"),
      testCase "1221 = 0"      $  0 @?= (subject "1221"),
      testCase "123425 = 4"    $  4 @?= (subject "123425"),
      testCase "123123 = 12"   $ 12 @?= (subject "123123"),
      testCase "12131415 = 4"  $  4 @?= (subject "12131415")
    ]
  where
    subject input =
      sumOfPairwiseEqual ((length input) `div` 2) $ parseInput input
