import Day4
import Helpers

main :: IO ()
main = do
  input <- parseInput <$> readInput "Day4"

  putStrLn $ show  $ length $ filter isValidPassphrase input
  putStrLn $ show  $ length $ filter isValidAnagramPassphrase input
