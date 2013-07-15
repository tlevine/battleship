battleship1d :: Num n => (n -> n) -> n -> n -> n -> n
battleship1d f a b prevCenters slopes
  | distanceToCenter < 0.01 = f $ head prevCenters
  | otherwise = maximum $ battleship1d f a b centers slopes
  where
    center = ((a + b) / 2)
    centers = center:prevCenters
    expectedPlanar = ((f a) + (f b)) / 2
    distanceToCenter = (b - a) / 2

-- battleship f 3 5 []
