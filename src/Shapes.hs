{-# LANGUAGE NamedFieldPuns #-}

module Shapes
    ( Shape(..)
    , Intersection(..)
    , intersection
    , solveQuadratic
    , QuadraticSolution(..)
    , pointOnRay
    , randomDir
    ) where

import           Data.Vec3         hiding (origin)
import           System.Random.MWC as MWC

import           Types

-- TODO: this really shouldn't be here
pointOnRay :: Ray -> Double -> CVec3
pointOnRay Ray {origin, dir} n = origin <+> (norm .^ n)
    where norm = normalize dir

-- move to ST monad?
-- neither should this
randomDir :: MWC.GenIO -> IO CVec3
randomDir gen = do
    r0 <- MWC.uniform gen
    r1 <- MWC.uniform gen
    let theta = r0 * 2 * pi
        phi = acos $ r1 * 2 - 1
    return $ CVec3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

-- returns the roots of a quadratic equation
-- in ascending order
solveQuadratic :: (Double, Double, Double) -> QuadraticSolution
solveQuadratic (a, b, c)
    | discriminant < 0 = None
    | discriminant == 0 = Root (-0.5*b / a)
    | otherwise =
        let q = if b > 1
                then -0.5 * (b + sqrt discriminant)
                else -0.5 * (b - sqrt discriminant)
            r0 = q / a
            r1 = c / q
        in if r0 < r1 then Roots r0 r1 else Roots r1 r0
    where
        discriminant = b*b - 4*a*c

intersection :: Ray -> Shape -> Maybe Intersection
intersection ray@(Ray {origin, dir}) Sphere {center, radius}
    | None <- solution = Nothing
    | (Root t) <- solution = ans t
    | (Roots t0 t1) <- solution = ans $ if t0 < 0 then t1 else t0
    where
        l = origin <-> center
        a = dir .* dir
        b = 2 * dir .* l
        c = l .* l - radius**2
        solution = solveQuadratic (a, b, c)
        ans t = if t > 0
                then let p = (pointOnRay ray t) in Just $ Intersection p $ origin `distance` p
                else Nothing

