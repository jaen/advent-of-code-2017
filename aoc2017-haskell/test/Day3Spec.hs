module Day3Spec (
  matchingSpiralDistance
) where

import Test.Tasty
import Test.Tasty.HUnit

import Day3

matchingSpiralDistance :: TestTree
matchingSpiralDistance =
  testGroup "Testing distance for spiralDistance" $ [
    testCase "spiralDistance    1 =  0" $ (spiralDistance    1) @?=  0,
    testCase "spiralDistance   12 =  3" $ (spiralDistance   12) @?=  3,
    testCase "spiralDistance   23 =  2" $ (spiralDistance   23) @?=  2,
    testCase "spiralDistance 1024 = 31" $ (spiralDistance 1024) @?= 31
  ]
