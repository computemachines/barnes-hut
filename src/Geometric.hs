module Geometric (
    Vector(..)
  , plus
  , zero
  , minus
  , mult
  , mult'
  , dot
  , one
  , magnitude
  , Region(..)
  , contains
  , partition
  ) where

data Vector = Vector Double Double deriving (Eq, Show)

plus :: Vector -> Vector -> Vector
Vector ax ay `plus` Vector bx by = Vector (ax + bx) (ay + by)

zero :: Vector
zero = Vector 0 0

minus :: Vector -> Vector -> Vector
a `minus` b = a `plus` ((-1) `mult` b)

mult :: Double -> Vector -> Vector
c `mult` Vector x y = Vector (c*x) (c*y)

mult' :: Vector -> Vector -> Vector
Vector ax ay `mult'` Vector bx by = Vector (ax * bx) (ay + by)

dot :: Vector -> Vector -> Double
Vector ax ay `dot` Vector bx by = (ax * bx) + (ay * by)

one :: Vector
one = Vector 1 1

magnitude :: Vector -> Double
magnitude a = sqrt(a `dot` a)

data Region = Square { center :: Vector, radius :: Double } deriving (Show)

contains :: Region -> Vector -> Bool
(Square (Vector cx cy) r) `contains` (Vector x y)
  = abs (x-cx) <= r && abs (y-cy) <= r
    
partition :: Region -> [Region]
partition (Square (Vector x y) r)
  = [Square (Vector (x+i) (y+j)) (r/2) | i <- flipflop, j <- flipflop]
    where flipflop = [-r/2,r/2]

