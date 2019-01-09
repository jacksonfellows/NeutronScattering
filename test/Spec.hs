module Main where

import           Test.Tasty

import qualified Tests.AABB
import qualified Tests.BVHAccelerationStructure

main :: IO ()
main = defaultMain $ testGroup "NeutronScattering"
    [ Tests.AABB.tests
    , Tests.BVHAccelerationStructure.tests
    ]
