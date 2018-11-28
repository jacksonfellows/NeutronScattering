module Main where

import Test.Tasty

import qualified Tests.Sphere

main :: IO ()
main = defaultMain $ testGroup "NeutronScattering"
    [ Tests.Sphere.tests
    ]
