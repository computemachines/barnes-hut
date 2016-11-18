module Geometric (
  Vector(..)
  ,magnitude
  ,Region(..)
  ,contains
  ,partition
  ) where

data Vector = Vector Double Double deriving (Eq, Show)

plus :: Vector -> Vector -> Vector
Vector ax ay `plus` Vector bx by = Vector (ax + bx) (ay + by)

magnitude :: Vector -> Double
magnitude (Vector a b) = sqrt(a*a + b*b)

data Region = Square { center :: Vector, radius :: Double } deriving (Show)

contains :: Region -> Vector -> Bool
(Square (Vector cx cy) r) `contains` (Vector x y)
  = abs (x-cx) <= r && abs (y-cy) <= r
    
partition :: Region -> [Region]
partition (Square (Vector x y) r)
  = [Square (Vector (x+i) (y+j)) (r/2) | i <- flipflop, j <- flipflop]
    where flipflop = [-r/2,r/2]

