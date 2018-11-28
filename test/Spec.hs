module Main where

import Test.Tasty

import qualified Tests.Sphere
import qualified Tests.AABB
import qualified Tests.BVH

main :: IO ()
main = defaultMain $ testGroup "NeutronScattering"
    [ Tests.Sphere.tests
    , Tests.AABB.tests
    , Tests.BVH.tests
    ]
