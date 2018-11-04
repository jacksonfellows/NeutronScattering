module Tests.Shapes
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Shapes

tests :: TestTree
tests = testGroup "Shapes" [unitTests]

unitTests = testGroup "Unit Tests"
    [ testCase "No solutions" $
        solveQuadratic (1, 2, 3) @?= None

    , testCase "One root" $
        solveQuadratic (1, 0, 0) @?= Root 0
        
    , testCase "Two roots" $
        solveQuadratic (1, 2, (-3)) @?= Roots (-3) 1
    ]
