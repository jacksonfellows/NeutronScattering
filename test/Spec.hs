module Main where

import Test.Tasty

import qualified Tests.Sphere
import qualified Tests.AABB

main :: IO ()
main = defaultMain $ testGroup "NeutronScattering"
    [ Tests.Sphere.tests
    , Tests.AABB.tests
    ]
