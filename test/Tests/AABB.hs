module Tests.AABB
    ( tests
    , PosVec3(..)
    , NegVec3(..)
    , UnitVector(..)
    ) where

import           Linear.Metric         (norm)
import           Linear.V3
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           AABB
import           Ray

tests :: TestTree
tests = testGroup "AABB" [intersectsTests, cAndUTests]

intersectsTests = testGroup "intersects" [unitTests, propertyTests]

unitTests = testGroup "Unit Tests"
    [ testCase "Does not intersect" $
        upRay `intersects` (aabb (V3 1 0 0) (V3 2 1 1)) @?= False

    , testCase "Inside AABB" $
        upRay `intersects` (aabb (V3 (-1) (-1) (-1)) (V3 1 1 1)) @?= True

    , testCase "Outside AABB" $
        upRay `intersects` (aabb (V3 (-1) (-1) 1) (V3 1 1 2)) @?= True
    ]
    where upRay = buildRay (V3 0 0 0) (V3 0 0 1)

newtype PosVec3 = MkPosVec3 (V3 Double)
    deriving (Show)

instance QC.Arbitrary PosVec3 where
    arbitrary = do
        Positive x <- arbitrary
        Positive y <- arbitrary
        Positive z <- arbitrary
        return $ MkPosVec3 $ V3 x y z

newtype NegVec3 = MkNegVec3 (V3 Double)
    deriving (Show)

instance QC.Arbitrary NegVec3 where
    arbitrary = do
        MkPosVec3 pos <- arbitrary
        return $ MkNegVec3 $ pos * (-1)

newtype UnitVector = MkUnitVector (V3 Double)
    deriving (Show)

-- copied from randomDir
instance QC.Arbitrary UnitVector where
    arbitrary = do
        r0 <- choose (0,1)
        r1 <- choose (0,1)
        let theta = r0 * 2 * pi
            phi = acos $ r1 * 2 - 1
        return $ MkUnitVector $ V3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

propertyTests = testGroup "Property Tests"
    [ QC.testProperty "All unit vectors generated for testing should have a norm of 1" $
        \(MkUnitVector dir) -> norm dir - 1 < 1e-5

    , QC.testProperty "All rays with origin (0,0,0) intersect an aabb with an all - min and all + max" $
        \(MkUnitVector dir) (MkNegVec3 min) (MkPosVec3 max) -> buildRay (V3 0 0 0) dir `intersects` (aabb min max)

    , QC.testProperty "All positive rays with origin (0,0,0) should not intersect an aabb with a - min and - max" $
        \(MkPosVec3 dir) (MkNegVec3 b0) (MkNegVec3 b1) -> not $ buildRay (V3 0 0 0) dir `intersects` (aabb b0 b1)
    ]

cAndUTests = testGroup "contains and union" [cAndUPropertyTests]

-- TODO: not only negative mins and maxes
instance Arbitrary AABB where
    arbitrary = do
        MkNegVec3 min <- arbitrary
        MkPosVec3 max <- arbitrary
        return $ aabb min max

cAndUPropertyTests = testGroup "Property Tests"
    [ QC.testProperty "if a contains b then b does not contain a" $
        \b (MkNegVec3 neg) (MkPosVec3 pos) -> let a = aabb (getMin b + neg) (getMax b + pos)
                                              in a `contains` b == True && b `contains` a == False

    , QC.testProperty "a union contains both of its members" $
        \a b -> (union a b) `contains` a && (union a b) `contains` b
    ]
