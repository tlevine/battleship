battleship1d :: Num n => (n -> n) -> n -> n -> n -> n
battleship1d f a b prevCenters slopes
  | distanceToCenter < 0.01 = f $ head prevCenters
  | otherwise = maximum $ map (\c -> battleship1d f a b centers slopes
  where
    center = ((a + b) / 2)
    centers = center:prevCenters
    expectedPlanar = ((f a) + (f b)) / 2
    distanceToCenter = (b - a) / 2

-- battleship f 3 5 []


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
