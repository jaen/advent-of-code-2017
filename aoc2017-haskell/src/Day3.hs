module Day3 (
  spiralDistance,
  firstSpiralSumBiggerThan
) where

import qualified Data.Map as Map
import Data.Map (Map, (!?))

import Control.Monad
import Data.STRef.Lazy
import Control.Monad.ST.Lazy
import Data.Maybe (fromMaybe)

spiralDistance :: Int -> Int
spiralDistance 1 = 0
spiralDistance a = (pred rungNumber) + sidewaysOffset
  where
    maxRungValues = map (^ 2) [1, 3..]
    rungNumber    = fst $ head $ dropWhile (\(_, x) -> x < a) $ zip [1..] maxRungValues

    sideSizes = [rungNumber - 1 , rungNumber - 2, rungNumber - 1, rungNumber - 2]
    sidewaysOffsets = drop 1 $ cycle $ concatMap (\x -> (reverse [1..x]) ++ [0..x] ) sideSizes

    sidewaysOffset = sidewaysOffsets !! pred (a - (maxRungValues !! (rungNumber - 2)))

spiral :: [(Int, Int)]
spiral = scanl moveStep (0, 0) steps
  where
    -- 2*x + 2*(x - 2) -> 4*(x - 1) -> 4*x and start range one lower
    rungElementCount = 1 : map (4*) [2, 4..]
    directions = [(0, 1), (-1, 0), (0, -1), (1, 0)]
    -- move right, then around the rung, stopping just before entering the same element
    directionsForRung n = (1, 0) : (take (4*n - 1) $ drop 1 $ cycle $ concatMap (take n . repeat) directions)
    steps = concatMap directionsForRung [2, 4..]
    moveStep (x, y) (stepX, stepY) = (x + stepX, y + stepY)

neighboursOf :: (Int, Int) -> [(Int, Int)]
neighboursOf (pX, pY) = map (\(x,y) -> (pX+x, pY+y)) surroundingSquare
  where
    surroundingSquare = [(-1,-1), (-1,0), (-1,1), (0,1), (1,1), (1,0), (1,-1), (0,-1)]

firstSpiralSumBiggerThan :: Int -> Int
firstSpiralSumBiggerThan a = head $ dropWhile (< a) spiralSum
  where
    neighbourSum :: Map (Int, Int) Int -> (Int, Int) -> Int
    neighbourSum map' position =
      let
        values = map (fromMaybe 0 . (map' !?)) $ position : neighboursOf position
      in
        sum values

    spiralSum :: [Int]
    spiralSum = reverse $ runST $ do
      map' <- newSTRef $ Map.fromList [((0,0), 1)]
      result <- newSTRef []

      forM_ (take a spiral) $ \position -> do
        map'' <- readSTRef map'

        let sum = (neighbourSum map'' position)

        writeSTRef map' $ Map.insert position sum map''
        modifySTRef result (sum :)

      readSTRef result
