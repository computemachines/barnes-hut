{-# LANGUAGE TemplateHaskell #-}

import NBody

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Applicative

instance Arbitrary Vector where
  arbitrary = Vector <$> arbitrary <*> arbitrary

instance Arbitrary Region where
  arbitrary = Square <$> arbitrary <*> arbitrary

-- deepCheck = quickCheckWith stdArgs { maxSuccess = 1000 }

prop_VectorAddId v = v + 0 == v
prop_VectorMultId v = v * Vector 1 1 == v
prop_VectorMagPos v = classify (magnitude v == 0) "trivial" $ magnitude v >= 0
prop_VectorTriangleIneq a b = magnitude (a+b) <= magnitude a + magnitude b
prop_RegionContainsCenter region@(Square center r) = region `contains` center
prop_RegionPartitionCover region testVector
  = contains region testVector ==> any (`contains` testVector) $ partition region

return []
main :: IO (Bool)
main = $quickCheckAll
