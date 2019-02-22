module Tests.BVHAccelerationStructure ( tests ) where

import           Linear.V3
import           Test.Tasty
import           Test.Tasty.QuickCheck      as QC

import           AABB
import           BVHAccelerationStructure
import           Intersect
import           NaiveAccelerationStructure
import           Ray
import           System.Random              (Random)
import           Tests.AABB                 (UnitVector (..))
import           Triangle

tests :: TestTree
tests = testGroup "BVH" [propertyTests]

propertyTests = testGroup "Property Tests"
    [ QC.testProperty "All BVH nodes contain their children, and all BVH leafs contain their prims" $
        \(NonEmpty prims) -> let bvh = construct prims :: BVHStructure Triangle Double
                             in nodesContainChildren bvh

    , QC.testProperty "The same behavior as a naive implementation" $
        \(NonEmpty prims) -> let bvh = construct prims :: BVHStructure Triangle Double
                                 naive = construct prims :: NaiveStructure Triangle Double
                                 box = buildAABB bvh
                             in QC.forAll (raysInBox box) $
                                \ray -> fmap fst (intersect ray bvh) == fmap fst (intersect ray naive)
    ]

raysInBox :: (Arbitrary n, Random n, Floating n) => AABB n -> Gen (Ray n)
raysInBox box = do
    let (V3 minX minY minZ) = getMin box
        (V3 maxX maxY maxZ) = getMax box
    x <- choose (minX,maxX)
    y <- choose (minY,maxY)
    z <- choose (minZ,maxZ)
    (MkUnitVector dir) <- arbitrary
    return $ buildRay (V3 x y z) dir
