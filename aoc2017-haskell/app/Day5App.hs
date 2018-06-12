import Day5
import Helpers

main :: IO ()
main = do
    input <- parseInput <$> readInput "Day5"

    putStrLn $ show $ fastStepsToEscape succ                 input
    putStrLn $ show $ fastStepsToEscape conditionalIncrement input

  where
    conditionalIncrement offset = if offset >= 3 then pred offset else succ offset
