import NBody
import Geometric

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Applicative

main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Algebraic" [
        testProperty "Vector Addition Identity Element" prop_VectorAddId
      , testProperty "Vector Addition Associativity" prop_VectorAddAssoc
      , testProperty "Vector Addition Commutivity" prop_VectorAddComm
      , testProperty "Vector Addition Inverse" prop_VectorAddInv
      , testProperty "Scalar Multiplication Identity Element" prop_VectorMultId
      , testProperty "Scalar Multiplication Associativity" prop_VectorMultAssoc
      , testProperty "Elementwise Multiplication Identity Element" prop_VectorMultId'
      , testProperty "Elementwise Multiplication Associativity" prop_VectorMultAssoc'
      , testProperty "Elementwise Multiplication Commutivity" prop_VectorMultComm'
      , testProperty "Dot Product Commutivity" prop_VectorDotComm
      , testProperty "Dot Product Distributive over Addition" prop_VectorDotAddDist
        ],
  testGroup "Geometric" [
        testProperty "Magnitude NonNegative" prop_VectorMagNN
      , testProperty "Theorem: Triangle Inequality" prop_VectorTriangleIneq
      ]
  ]


instance Arbitrary Vector where
  arbitrary = Vector <$> arbitrary <*> arbitrary

instance Arbitrary Region where
  arbitrary = Square <$> arbitrary <*> arbitrary


-- Algebra tests
prop_VectorAddId a = zero `plus` a == a
prop_VectorAddAssoc a b c = plus a (plus b c) == plus (plus a b) c
prop_VectorAddComm a b = a `plus` b == b `plus` a
prop_VectorAddInv a = a `minus` a == zero
prop_VectorMultId a = 1 `mult` a == a
prop_VectorMultAssoc c1 c2 a = c1 `mult` (c2 `mult` a) == (c1*c2) `mult` a
prop_VectorMultId' v = mult' v one == v
prop_VectorMultAssoc' a b c = mult' a (mult' b c) == mult' (mult' a b) c
prop_VectorMultComm' a b = mult' a b == mult' b a
prop_VectorDotComm a b = a `dot` b == b `dot` a
prop_VectorDotAddDist a b c
  = dot a (b `plus` c) == (dot a b) + (dot a c)
  
prop_VectorMagNN v = classify (magnitude v == 0) "trivial" $ magnitude v >= 0
prop_VectorTriangleIneq a b = magnitude (plus a b) <= magnitude a + magnitude b
-- prop_RegionContainsCenter region@(Square center r) = region `contains` center
-- prop_RegionPartitionCover region testVector
--   = contains region testVector ==> any (`contains` testVector) $ partition region

