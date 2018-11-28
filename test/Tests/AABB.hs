module Tests.AABB
    ( tests ) where

import           Data.Vec3
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           AABB
import           Ray

tests :: TestTree
tests = testGroup "AABB" [intersectsTests]

intersectsTests = testGroup "intersects" [unitTests, propertyTests]

unitTests = testGroup "Unit Tests"
    [ testCase "Does not intersect" $
        upRay `intersects` (MkAABB (CVec3 1 0 0) (CVec3 2 1 1)) @?= False

    , testCase "Inside AABB" $
        upRay `intersects` (MkAABB (CVec3 (-1) (-1) (-1)) (CVec3 1 1 1)) @?= True

    , testCase "Outside AABB" $
        upRay `intersects` (MkAABB (CVec3 (-1) (-1) 1) (CVec3 1 1 2)) @?= True
    ]
    where upRay = MkRay (CVec3 0 0 0) (CVec3 0 0 1)

newtype PosVec3 = MkPosVec3 CVec3
    deriving (Show)

instance QC.Arbitrary PosVec3 where
    arbitrary = do
        Positive x <- arbitrary
        Positive y <- arbitrary
        Positive z <- arbitrary
        return $ MkPosVec3 $ CVec3 x y z

newtype NegVec3 = MkNegVec3 CVec3
    deriving (Show)

instance QC.Arbitrary NegVec3 where
    arbitrary = do
        MkPosVec3 pos <- arbitrary
        return $ MkNegVec3 $ invert pos

newtype UnitVector = MkUnitVector CVec3
    deriving (Show)

-- copied from randomDir
instance QC.Arbitrary UnitVector where
    arbitrary = do
        r0 <- choose (0,1)
        r1 <- choose (0,1)
        let theta = r0 * 2 * pi
            phi = acos $ r1 * 2 - 1
        return $ MkUnitVector $ CVec3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

propertyTests = testGroup "Property Tests"
    [ QC.testProperty "All unit vectors generated for testing should have a norm of 1" $
        \(MkUnitVector dir) -> norm dir - 1 < 1e-5

    , QC.testProperty "All rays with origin (0,0,0) intersect an aabb with an all - min and all + max" $
        \(MkUnitVector dir) (MkNegVec3 min) (MkPosVec3 max) -> MkRay (CVec3 0 0 0) (normalize dir) `intersects` (MkAABB min max)
    ]
