module Tests.Shapes
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Shapes

tests :: TestTree
tests = testGroup "Shapes" [solveQuadraticTests]

solveQuadraticTests = testGroup "solveQuadratic" [quadUnitTests, quadPropertyTests]

quadUnitTests = testGroup "Unit Tests"
    [ testCase "No solutions" $
        solveQuadratic (1, 2, 3) @?= None

    , testCase "One root" $
        solveQuadratic (1, 0, 0) @?= Root 0
        
    , testCase "Two roots" $
        solveQuadratic (1, 2, (-3)) @?= Roots (-3) 1
    ]

isAscending :: QuadraticSolution -> Bool
isAscending (Roots a b) = a < b
isAscending _ = True

quadPropertyTests = testGroup "Property Tests"
    [ QC.testProperty "Roots are always returned in ascending order" $
        \eqa -> isAscending $ solveQuadratic (eqa :: (Double, Double, Double))
    ]
