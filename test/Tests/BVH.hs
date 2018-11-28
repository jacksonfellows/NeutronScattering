module Tests.BVH
    ( tests ) where

import           Data.Vec3
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           AABB
import           BVH
import           Object
import           Shape
import           Sphere
import           Tests.AABB            (UnitVector (..))

objHelper = (flip MkObject) (MkMat id id "test")
leafHelper = buildLeaf . objHelper

tests :: TestTree
tests = testGroup "BVH" [unitTests, propertyTests]

unitTests = testGroup "Unit Tests"
    [ testCase "A single sphere with no intersection" $
        upRay `intersectBVH` (leafHelper (MkSphere (CVec3 0 0 (-10)) 9)) @?= Nothing

    , testCase "Inside a single sphere" $
        fmap fst (upRay `intersectBVH` (leafHelper (MkSphere (CVec3 0 0 0) 10))) @?= Just (MkIntersection (CVec3 0 0 10) 10)

    , testCase "Outside a single sphere" $
        fmap fst (upRay `intersectBVH` (leafHelper (MkSphere (CVec3 0 0 10) 5))) @?= Just (MkIntersection (CVec3 0 0 5) 5)
    ]
    where upRay = MkRay (CVec3 0 0 0) (CVec3 0 0 1)

-- TODO: move useful arbitrary instances like these into a central place
instance QC.Arbitrary Ray where
    arbitrary = do
        origin <- arbitrary
        MkUnitVector dir <- arbitrary
        return $ MkRay origin dir

instance QC.Arbitrary Sphere where
    arbitrary = do
        center <- arbitrary
        Positive radius <- arbitrary
        return $ MkSphere center radius

propertyTests = testGroup "Property Tests"
    [ QC.testProperty "ray `intersect` sphere == ray `intersect` AABB (sphere)" $
        \ray sphere -> ray `intersect` sphere == (fmap fst $ ray `intersectBVH` (leafHelper sphere))

     , QC.testProperty "the aabb of a branch contains its children" containsTest
    ]

containsTest :: [Sphere] -> Bool
containsTest [] = True
containsTest spheres = parentsContainChildren tree
    where (o:os) = map objHelper spheres
          tree = foldl addToBVH (buildLeaf o) os

parentsContainChildren :: BVHTree a -> Bool
parentsContainChildren (Leaf _ _) = True
parentsContainChildren (Branch aabb l r) = all pred [l,r]
    where pred = \c -> (aabb `contains` (getAABB c)) && (parentsContainChildren c)
