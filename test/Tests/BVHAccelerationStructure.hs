module Tests.BVHAccelerationStructure ( tests ) where

import           Data.Vec3
import           Test.Tasty
import           Test.Tasty.QuickCheck      as QC

import           AABB
import           BVHAccelerationStructure
import           Intersect
import           NaiveAccelerationStructure
import           Ray
import           Tests.AABB                 (UnitVector (..))
import           Triangle

tests :: TestTree
tests = testGroup "BVH" [propertyTests]

propertyTests = testGroup "Property Tests"
    [ QC.testProperty "All BVH nodes contain their children, and all BVH leafs contain their prims" $
        \(NonEmpty prims) -> let bvh = construct prims :: BVHStructure Triangle
                             in nodesContainChildren bvh

    , QC.testProperty "The same behavior as a naive implementation" $
        \(NonEmpty prims) -> let bvh = construct prims :: BVHStructure Triangle
                                 naive = construct prims :: NaiveStructure Triangle
                                 box = buildAABB bvh
                             in QC.forAll (raysInBox box) $
                                \ray -> fmap fst (intersect ray bvh) == fmap fst (intersect ray naive)
    ]

raysInBox :: AABB -> Gen Ray
raysInBox box = do
    let (minX,minY,minZ) = toXYZ $ getMin box
        (maxX,maxY,maxZ) = toXYZ $ getMax box
    x <- choose (minX,maxX)
    y <- choose (minY,maxY)
    z <- choose (minZ,maxZ)
    (MkUnitVector dir) <- arbitrary
    return $ MkRay (CVec3 x y z) dir
