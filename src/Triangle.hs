module Triangle
    ( Triangle
    , intersectTri
    , getCentroid
    ) where

import           Data.Vec3

import           Ray

type Triangle = (CVec3,CVec3,CVec3)

getCentroid :: Triangle -> CVec3
getCentroid (a,b,c) = (a <+> b <+> c) .^ (1/3)

-- moller-trumbore algorithm (copied from internet)
-- TODO: make it more haskelly (or, make it less haskelly be removing Maybe?)
-- TODO: for now, no culling -> make this dependent on whether I'm inside or outside an object?
intersectTri :: Ray -> Triangle -> Maybe Double
(MkRay o d) `intersectTri` (v0,v1,v2)
    | abs det < 1e-5 = Nothing
    | u < 0 || u > 1 = Nothing
    | v < 0 || u + v > 1 = Nothing
    | t < 0 = Nothing
    | otherwise = Just t
    where v0v1 = v1 <-> v0
          v0v2 = v2 <-> v0
          pvec = d >< v0v2
          det = v0v1 .* pvec
          invDet = 1 / det
          tvec = o <-> v0
          u = (tvec .* pvec) * invDet
          qvec = tvec >< v0v1
          v = (d .* qvec) * invDet
          t = (v0v2 .* qvec) * invDet

