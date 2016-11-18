import NBody

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck2

import Control.Applicative

main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Geometric" [
      testProperty "prop add identity" prop_VectorAddId
      , testProperty "prop " prop_VectorAddId
      , testProperty "magnitude is nonnegative" prop_VectorMagPosNN
      , testProperty "Triangle Inequality Theorem" prop_VectorTriangleIneq
      , testProperty "Necessary 
                        ]
  ]


instance Arbitrary Vector where
  arbitrary = Vector <$> arbitrary <*> arbitrary

instance Arbitrary Region where
  arbitrary = Square <$> arbitrary <*> arbitrary


-- Algebra tests
prop_VectorAddId a = zero +' a == a
prop_VectorAddAssoc a b = a +' b == b +' a
prop_VectorAddInv a = a -' a == zero
prop_VectorMultId v = v `dot` one == v
prop_VectorMultAssoc a b = a `dot` b == b `dot` a
prop_VectorDistribMultAdd a b c = a `dot` (b +' c) == (a `dot` b) +' (a `dot c)

prop_VectorMagPos v = classify (magnitude v == 0) "trivial" $ magnitude v >= 0
prop_VectorTriangleIneq a b = magnitude (a+b) <= magnitude a + magnitude b
prop_RegionContainsCenter region@(Square center r) = region `contains` center
prop_RegionPartitionCover region testVector
  = contains region testVector ==> any (`contains` testVector) $ partition region

