import Test.Tasty
import Test.Tasty.HUnit

import Day1Spec
import Day2Spec

main :: IO ()
main = do
  defaultMain $
    testGroup "Advent of Code 2017 Tests" [
      (testGroup "Day1 Tests" [nextDigit, halfwayListAwayDigit]),
      (testGroup "Day2 Tests" [extremaDifferenceChecksum, evenlyDivisibleChecksum])
    ]
