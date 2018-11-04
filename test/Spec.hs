module Main where

import Test.Tasty

import qualified Tests.Shapes

main :: IO ()
main = defaultMain $ testGroup "NeutronScattering"
    [ Tests.Shapes.tests
    ]
