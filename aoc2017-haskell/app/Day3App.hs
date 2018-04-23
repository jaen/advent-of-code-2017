import Day3
import Helpers

input = 265149

main :: IO ()
main = do
  let result    = spiralDistance input
  let sumResult = firstSpiralSumBiggerThan input

  putStrLn $ show result
  putStrLn $ show sumResult
