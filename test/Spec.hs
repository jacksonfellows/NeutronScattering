module Main where

import           Test.Tasty

import qualified Tests.AABB

main :: IO ()
main = defaultMain $ testGroup "NeutronScattering"
    [ Tests.AABB.tests ]
