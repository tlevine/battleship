import Data.Function (on)
import Data.List (sortBy)

battleship1d :: Num n => (n -> n) -> n -> n -> n -> n
battleship1d f a b prevBounds prevSlopes
  | distanceToCenter < 0.01 = f $ head prevCenters
  | otherwise = battleship1d f nextA nextB bounds slopes


candidates :: Num n => (n -> n) -> n -> n -> n -> n
candidates f a b prevBounds prevSlopes = sortBy (compare `on` snd) $ zip bounds expectedYields
  where
    bounds = (a,b):prevBounds
    slopes = ():prevSlopes
    expectedYields = map bounds (\(a,b) -> expectedYield f a b high slopes)

expectedYield :: Num n => (n -> n) -> n -> n -> n -> [n]
expectedYield f a b high slopes = averageGain * slopesAbove
  where
    planarHeight = ((f a) + (f b)) / 2
    minSlope = slopeNecessaryToBeANewPeak f a b high
    propAboveMinSlope = (length $ slopesAbove) / (length slopes)
    averageGain = (sum $ map (- minSlope) slopes) / (length slopes)
    slopesAbove = filter (>= minSlope) slopes

-- Slope necessary to be a new peak
slopeNecessaryToBeANewPeak :: Num n => (n -> n) -> n -> n -> n
slopeNecessaryToBeANewPeak f a b highWater = (highWater - ((f a) + (f b))/2) / ((b - a) / 2)
