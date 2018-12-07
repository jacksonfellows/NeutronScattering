module Main where

import           Test.Tasty

import qualified Tests.AABB
import qualified Tests.Mesh
import qualified Tests.Sphere

main :: IO ()
main = defaultMain $ testGroup "NeutronScattering"
    [ Tests.Sphere.tests
    , Tests.AABB.tests
    , Tests.Mesh.tests
    ]
