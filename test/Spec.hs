import NBody

import Test.QuickCheck

main :: IO ()
main = quickCheck prop_RegionContainsCenter


instance Arbitrary Vector where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Vector x y)

instance Arbitrary Region where
  arbitrary = do
    center <- arbitrary
    radius <- arbitrary
    return (Square center (abs radius))


prop_RegionContainsCenter region@(Square center r)
  = region `contains` center
