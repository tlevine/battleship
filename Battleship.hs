import qualified Data.Heap as H

type Candidates n = H.MaxPrioHeap n (n, n, Maybe n)
-- expectedyield (a, b, centerValue)

battleship1d :: Num n => (n -> n) -> n -> n -> n
battleship1d f a b = battleship1dInternal f a b [] [] []

battleship1dInternal :: Num n => (n -> n) -> n -> n -> Candidates n -> [n]
battleship1dInternal f prevA prevB high prevBounds prevSlopes prevCandidates
  | nextExpectedYield < 10 = high
  | otherwise = battleship1dInternal f a b bounds slopes
  where
    bounds = (prevA,prevB):prevBounds
    rise = 
    run =
    slopes = (rise/run):prevSlopes
    nextCandidates = candidates f prevA prevB high bounds slopes


candidates :: Num n => (n -> n) -> n -> n -> [n] -> [n] -> Candidates n -> Candidates n
candidates f a b high bounds slopes prevCandidates = 
  where
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
