import Day1
import Helpers

main :: IO ()
main = do
  input <- parseInput <$> readInput "Day1"

  let halfInputLength = ((length input) `div` 2)

  putStrLn $ show $ sumOfPairwiseEqual 1 input
  putStrLn $ show $ sumOfPairwiseEqual halfInputLength $ input
