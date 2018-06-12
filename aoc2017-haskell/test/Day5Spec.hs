module Day5Spec (
  testNextState,
  testStepsToEscape,
  testSeqStepsToEscape,
  testFastStepsToEscape
) where

import Test.Tasty
import Test.Tasty.HUnit

import Day5

testNextState ::TestTree
testNextState =
  testGroup "Testing `nextState` function" $ isValidStateProgression [
    stateOf [ 0, 3, 0, 1, -3 ] 0,
    stateOf [ 1, 3, 0, 1, -3 ] 0,
    stateOf [ 2, 3, 0, 1, -3 ] 1,
    stateOf [ 2, 4, 0, 1, -3 ] 4,
    stateOf [ 2, 4, 0, 1, -2 ] 1,
    stateOf [ 2, 5, 0, 1, -2 ] 5
  ]
    where
      isValidStateProgression states = map isValidProgression $ paired states
      paired states                  = zip states $ drop 1 states
      isValidProgression (from, to)  =
        let
          description    = "step from " ++ (show (instructions from)) ++ " to " ++ (show (instructions to))
          offsetChangeFn = succ
        in
          testCase description $ (nextState offsetChangeFn from) @?= to

testStepsToEscape :: TestTree
testStepsToEscape =
  testGroup "Testing `stepsToEscape` function" $ [
    testCase description $ (stepsToEscape offsetChangeFn initialState) @?= expectedStepsToEscape
  ]
    where
      initialState          = stateOf [ 0, 3, 0, 1, -3 ] 0
      expectedStepsToEscape = 5
      offsetChangeFn        = succ
      description           = "escapes " ++ (show initialState) ++ " in " ++ (show expectedStepsToEscape) ++ " steps"

testSeqStepsToEscape :: TestTree
testSeqStepsToEscape =
  testGroup "Testing `seqStepsToEscape` function" $ [
    stepCountTest initialState 5 succ,
    stepCountTest initialState 10 (\offset -> if offset >= 3 then pred offset else succ offset)
  ]
    where
      stepCountTest state expectedSteps offsetChangeFn =
        testCase (description state expectedSteps) $ (seqStepsToEscape offsetChangeFn state) @?= expectedSteps

      initialState          = stateOf [ 0, 3, 0, 1, -3 ] 0
      description state steps = "escapes " ++ (show state) ++ " in " ++ (show steps) ++ " steps"



testFastStepsToEscape :: TestTree
testFastStepsToEscape =
  testGroup "Testing `fastStepsToEscape` function" $ [
    stepCountTest initialState 5 succ,
    stepCountTest initialState 10 (\offset -> if offset >= 3 then pred offset else succ offset)
  ]
    where
      stepCountTest state expectedSteps offsetChangeFn =
        testCase (description state expectedSteps) $ (fastStepsToEscape offsetChangeFn state) @?= expectedSteps

      initialState          = stateOf [ 0, 3, 0, 1, -3 ] 0
      description state steps = "escapes " ++ (show state) ++ " in " ++ (show steps) ++ " steps"

