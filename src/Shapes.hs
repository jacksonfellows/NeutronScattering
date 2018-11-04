{-# LANGUAGE NamedFieldPuns #-}

module Shapes
    ( Shape(..)
    , Intersection(..)
    , intersection
    ) where

import Linear

-- TODO parametrize these types?

-- TODO: shapes are meshes with bounding boxes?
data Shape = Sphere
    { center :: V3 Double
    , radius :: Double
    }
    deriving (Show, Read, Eq, Ord)

data QuadraticSolution = Roots Double Double | Root Double | None
    deriving (Show, Read)

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

data Intersection = Intersection
    { point :: V3 Double
    , distanceFrom :: Double
    }
    deriving (Show, Eq, Read)

-- Intersections can be ordered based on closeness
instance Ord Intersection where
    (Intersection _ a) <= (Intersection _ b) = a <= b

intersection :: Ray Double -> Shape -> Maybe Intersection
intersection ray@(Ray {origin, dir}) Sphere {center, radius}
    | None <- solution = Nothing
    | (Root t) <- solution = ans t
    | (Roots t0 t1) <- solution = ans $ if t0 < 0 then t1 else t0
    where
        l = origin - center
        a = dir `dot` dir
        b = 2 * dir `dot` l
        c = l `dot` l - radius**2
        solution = solveQuadratic (a, b, c)
        ans t = if t > 0
                then let p = (pointOnRay ray t) in Just $ Intersection p $ origin `distance` p
                else Nothing

