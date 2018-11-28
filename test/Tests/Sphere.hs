{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Sphere
    ( tests
    ) where

import           Data.Vec3
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Ray
import           Shape
import           Sphere

tests :: TestTree
tests = testGroup "Sphere" [solveQuadraticTests, intersectionTests]

solveQuadraticTests = testGroup "solveQuadratic" [quadUnitTests, quadPropertyTests]

quadUnitTests = testGroup "Unit Tests"
    [ testCase "No solutions" $
        solveQuadratic (1, 2, 3) @?= None

    , testCase "One root" $
        solveQuadratic (1, 0, 0) @?= Root 0

    , testCase "Two roots" $
        solveQuadratic (1, 2, (-3)) @?= Roots (-3) 1
    ]

isAscending :: QuadSolution -> Bool
isAscending (Roots a b) = a < b
isAscending _           = True

quadPropertyTests = testGroup "Property Tests"
    [ QC.testProperty "Roots are always returned in ascending order" $
        \eqa -> isAscending $ solveQuadratic (eqa :: (Double, Double, Double))
    ]

intersectionTests = testGroup "intersection" [intUnitTests, intPropertyTests]

intUnitTests = testGroup "Unit Tests"
    [ testCase "No intersection" $
        upRay `intersect` (MkSphere (CVec3 0 0 (-10)) 9) @?= Nothing

    , testCase "Inside Sphere" $
        upRay `intersect` (MkSphere (CVec3 0 0 0) 10) @?= Just (MkIntersection (CVec3 0 0 10) 10)

    , testCase "Outside Sphere" $
        upRay `intersect` (MkSphere (CVec3 0 0 10) 5) @?= Just (MkIntersection (CVec3 0 0 5) 5)
    ]
    where upRay = MkRay (CVec3 0 0 0) (CVec3 0 0 1)

-- TODO: I don't really understand what's best here

-- A ray and sphere guaranteed to intersect
data GuaranteedIntersection = GuaranteedIntersection Ray Sphere
    deriving (Show)

-- TODO: don't only generate rays pointed towards the center of the sphere
instance QC.Arbitrary GuaranteedIntersection where
    arbitrary = do
        origin :: CVec3 <- arbitrary
        center :: CVec3 <- arbitrary
        Positive radius <- arbitrary
        let dir = if center == origin then (CVec3 0 0 1) else center <-> origin
        return $ GuaranteedIntersection (MkRay origin (normalize dir)) (MkSphere center radius)


intPropertyTests = testGroup "Property Tests"
    [ QC.testProperty "Rays and spheres constructed to intersect always do" $
        \(GuaranteedIntersection ray sphere) -> ray `intersect` sphere /= Nothing

    , QC.testProperty "distanceFrom = origin `distance` point" $
        \(GuaranteedIntersection ray sphere) -> let Just (MkIntersection point dist) = ray `intersect` sphere in dist - (getO ray) `distance` point < 1e-5
    ]
