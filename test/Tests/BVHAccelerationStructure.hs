module Tests.BVHAccelerationStructure ( tests ) where

import           Data.Vec3
import           Test.Tasty
import           Test.Tasty.QuickCheck    as QC

import           BVHAccelerationStructure
import           Intersect
import           Triangle

tests :: TestTree
tests = testGroup "BVH" [propertyTests]

propertyTests = testGroup "Property Tests"
    [ QC.testProperty "All BVH nodes contain their children, and all BVH leafs contain their prims" $
        \(NonEmpty prims) -> let bvh = construct prims :: BVHStructure Triangle
                             in nodesContainChildren bvh
    ]
