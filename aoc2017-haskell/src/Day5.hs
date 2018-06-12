{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day5 (
  nextState,
  State (..),
  stepsToEscape,
  seqStepsToEscape,
  fastStepsToEscape,
  parseInput,
  stateOf
) where

import Control.Lens (over, element, (.~))

import System.IO.Unsafe

import Data.Array.Unboxed
import Data.Array.ST

import Debug.Trace

import GHC.Generics (Generic, Generic1)
import qualified Data.Sequence as Seq

-- import Data.Array.IArray

import Control.Monad
import Data.STRef.Lazy
import Control.Monad.ST.Lazy
-- import Control.Monad.ST
import Data.Maybe (fromMaybe)

import qualified Control.DeepSeq as DeepSeq

data State = State {
  instructions :: [Int], -- Array Int Int,
  location     :: Int }

deriving instance Show State
deriving instance Eq State
deriving instance Generic State
-- deriving instance Generic1 State
deriving instance DeepSeq.NFData State

stateOf :: [Int] -> Int -> State
stateOf instructions location =
  State { instructions, location }
    -- where
    --   instructions = listArray (0, (pred $ length instructionList)) instructionList

nextState :: (Int -> Int) -> State -> State
nextState offsetChangeFn State { instructions, location } = newState
  where
    instruction     = instructions !! location
    newInstructions = over (element location) offsetChangeFn instructions
    newState        = State { instructions = newInstructions,
                              location = location + instruction }

stepsToEscape :: (Int -> Int) -> State -> Int
stepsToEscape offsetChangeFn initial = length $ takeWhile notEscaped states
  where
    maxLocation                   = length $ instructions initial
    states                        = iterate (nextState offsetChangeFn) initial
    notEscaped State { location } = location < maxLocation && location >= 0

--

type SeqState = (Seq.Seq Int, Int)

seqNextState :: (Int -> Int) -> SeqState -> SeqState
seqNextState offsetChangeFn (instructionSeq, location) = nextState
  where
    instruction        = Seq.index instructionSeq location
    nextInstructionSeq = Seq.adjust offsetChangeFn location instructionSeq
    nextLocation       = location + instruction
    nextState          = (nextInstructionSeq, nextLocation)

seqStepsToEscape :: (Int -> Int) -> State -> Int
seqStepsToEscape offsetChangeFn initial = length $ takeWhile notEscaped states
  where
    initialTuple             = (Seq.fromList $ instructions initial, location initial)
    maxLocation              = Seq.length $ fst initialTuple
    states                   = iterate (seqNextState offsetChangeFn) initialTuple
    notEscaped (_, location) = location < maxLocation && location >= 0

--

fastStepsToEscape :: (Int -> Int) -> State -> Int
fastStepsToEscape offsetChangeFn State { instructions, location } = magic instructions location
  where
    magic instructions location = runST action

    action :: forall s . ST s Int
    action = do
      state <- newListArray (0, (pred $ length instructions)) instructions :: ST s (STArray s Int Int)
      location <- newSTRef location
      steps <- newSTRef 0

      let loop = do {
        currentLocation <- readSTRef location;
        currentSteps    <- readSTRef steps;

        when (notEscaped currentLocation) $ do {
          instruction <- readArray state currentLocation;

          modifySTRef location (+ instruction);
          writeArray state currentLocation (offsetChangeFn instruction);

          modifySTRef steps succ;

          loop;
        }
      }

      loop

      readSTRef steps

    maxLocation         = length instructions
    notEscaped location = location < maxLocation && location >= 0

parseInput :: String -> State
parseInput input = stateOf instructions 0
  where
    instructions = map (read :: String -> Int) $ lines input
