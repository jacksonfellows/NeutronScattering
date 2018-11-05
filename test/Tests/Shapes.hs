{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Shapes
    ( tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Shapes
import Linear

tests :: TestTree
tests = testGroup "Shapes" [solveQuadraticTests, intersectionTests]

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

intersectionTests = testGroup "intersection" [intUnitTests, intPropertyTests]

intUnitTests = testGroup "Unit Tests"
    [ testCase "No intersection" $
        intersection upVec Sphere {center = V3 0 0 (-10), radius = 9} @?= Nothing

    , testCase "Inside Sphere" $
        intersection upVec Sphere {center = V3 0 0 0, radius = 10} @?= Just Intersection {point = V3 0 0 10, distanceFrom = 10}

    , testCase "Outside Sphere" $
        intersection upVec Sphere {center = V3 0 0 10, radius = 5} @?= Just Intersection {point = V3 0 0 5, distanceFrom = 5}
    ]
    where upVec = Ray {origin = V3 0 0 0, dir = V3 0 0 1}

-- TODO: I don't really understand what's best here

-- A ray and sphere guaranteed to intersect
data GuaranteedIntersection = GuaranteedIntersection (Ray Double) Shape
    deriving (Show)

-- TODO: don't only generate rays pointed towards the center of the sphere
instance QC.Arbitrary GuaranteedIntersection where
    arbitrary = do
        origin <- arbitrary
        center <- arbitrary
        Positive radius <- arbitrary
        let dir = if center == origin then V3 0 0 1 else center - origin
        return $ GuaranteedIntersection Ray {origin, dir} Sphere {center, radius}


intPropertyTests = testGroup "Property Tests"
    [ QC.testProperty "Rays and spheres constructed to intersect always do" $
        \(GuaranteedIntersection ray sphere) -> intersection ray sphere /= Nothing

    , QC.testProperty "distanceFrom = origin `distance` point" $
        \(GuaranteedIntersection ray sphere) -> let Just Intersection {..} = intersection ray sphere in distanceFrom == (origin ray) `distance` point
    ]
