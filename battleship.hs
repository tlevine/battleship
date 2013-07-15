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
expectedYield f a b highWater slopes = 
  where
    planarHeight = ((f a) + (f b)) / 2
    minSlope = ((f b) - (f a)) / (b - a)

-- Slope necessary to be a new peak
slopeNecessaryToBeANewPeak :: Num n => (n -> n) -> n -> n -> n
slopeNecessaryToBeANewPeak f a b high = (high - ((f a) + (f b))/2) / ((b - a) / 2)
