module Sphere
    ( Sphere(..)
    , solveQuadratic
    , QuadSolution(..)
    ) where

import           Data.Vec3

import           Shape

data Sphere = MkSphere
    { getCenter :: CVec3
    , getRad    :: Double
    } deriving (Show)


data QuadSolution = Roots Double Double | Root Double | None
    deriving (Show, Eq)

-- returns the roots of a quadratic equation
-- in ascending order
solveQuadratic :: (Double, Double, Double) -> QuadSolution
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

instance IntersectionPrimitive Sphere where
    (MkRay o d) `intersectPrim` (MkSphere center r)
        | None <- solution = Nothing
        | (Root t) <- solution = ans t
        | (Roots t0 t1) <- solution = ans $ if t0 < 0 then t1 else t0
        where l = o <-> center
              a = d .* d
              b = 2 * d .* l
              c = l .* l - r**2
              solution = solveQuadratic (a, b, c)
              ans t = if t > 0
                      then Just $ MkIntersection (o <+> (d .^ t)) t
                      else Nothing

    buildAABBPrim (MkSphere c r) = MkAABB (c <-> rVec) (c <+> rVec)
        where rVec = CVec3 r r r
