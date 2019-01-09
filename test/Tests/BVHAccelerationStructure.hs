module Tests.BVHAccelerationStructure ( tests ) where

import           Data.Vec3
import           Test.Tasty
import           Test.Tasty.QuickCheck    as QC

import           AccelerationStructure
import           BVHAccelerationStructure

tests :: TestTree
tests = testGroup "BVH" [propertyTests]

propertyTests = testGroup "Property Tests"
    [ QC.testProperty "All BVH nodes contain their children, and all BVH leafs contain their tris" $
        \(NonEmpty tris) -> let bvh = build tris :: BVHStructure
                 in nodesContainChildren bvh
    ]
