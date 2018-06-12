import Day5
import Helpers

import Criterion.Main

-- main :: IO ()
-- main = do
--   input <- parseInput <$> readInput "Day5"

--   putStrLn $ show  $ seqStepsToEscape input succ
--   putStrLn $ show  $ seqStepsToEscape input (\offset -> if offset >= 3 then pred offset else succ offset)


setupEnv :: IO State
setupEnv = do
  parseInput <$> readInput "Day5"

main :: IO ()
main = defaultMain [
    -- notice the lazy pattern match here!
    env setupEnv $
      \ ~input -> bgroup "main" [
        bgroup "succ" [
          -- bench "stepsToEscape"     $ whnf (stepsToEscape succ)     input,
          bench "fastStepsToEscape" $ whnf (fastStepsToEscape succ) input,
          bench "seqStepsToEscape"  $ whnf (seqStepsToEscape succ)  input
        ]--,
        -- bgroup "conditional" [
        --   -- bench "stepsToEscape" $ whnf $ (stepsToEscape conditionalIncrement) input,
        --   bench "fastStepsToEscape" $ whnf (fastStepsToEscape conditionalIncrement) input,
        --   bench "seqStepsToEscape"  $ whnf (seqStepsToEscape conditionalIncrement)  input
        -- ]
      ]
    ]
  where
    conditionalIncrement offset = if offset >= 3 then pred offset else succ offset