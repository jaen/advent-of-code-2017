import Day2
import Helpers

main :: IO ()
main = do
  input <- parseInput <$> readInput "Day2"

  let result          = checksum extremaDifference input
  let divisibleResult = checksum evenlyDivisible input

  putStrLn $ show result
  putStrLn $ show divisibleResult
