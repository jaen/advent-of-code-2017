module Day1App where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- import Control.Bind ((=<<))
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (length)

import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Run (runBaseEff)

import Day1 (parseInput, sumOfPairwiseEqual)
-- import Helpers

readInput :: forall e . String -> Eff (err :: EXCEPTION, fs :: FS | e) String
readInput day =
  readTextFile UTF8 $ "../input/" <> day <> "Input"

main :: forall e . Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = runBaseEff do
  input <- parseInput <$> readInput "Day1"

  let halfInputLength = ((length input) `div` 2)

  log $ show $ sumOfPairwiseEqual 1 input
  log $ show $ sumOfPairwiseEqual halfInputLength $ input
