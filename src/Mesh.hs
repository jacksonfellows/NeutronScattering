{-# LANGUAGE BangPatterns #-}

module Mesh
    ( Mesh(..)
    , fromTris
    -- for debugging
    , intersectTri
    ) where

import           Control.Applicative  ((<|>))
import           Data.Vec3
import qualified Data.Vector.Unboxed  as V
import           GHC.Float            (float2Double)
import           Graphics.Formats.STL

import           Shape

data Mesh = MkMesh { getTris :: V.Vector (CVec3,CVec3,CVec3) }

maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMin a b = min <$> a <*> b <|> a <|> b

instance Shape Mesh where
    ray@(MkRay o d) `intersect` (MkMesh vecs) = do
        t <- V.foldl (\min tri -> maybeMin min (ray `intersectTri` tri)) Nothing vecs
        return $ MkIntersection (o <+> (d .^ t)) t

    buildAABB _ = undefined

-- moller-trumbore algorithm (copied from internet)
-- TODO: make it more haskelly (or, make it less haskelly be removing Maybe?)
-- TODO: for now, no culling -> make this dependent on whether I'm inside or outside an object?
intersectTri :: Ray -> (CVec3,CVec3,CVec3) -> Maybe Double
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

-- TODO: bad, very bad
fromTris :: [Triangle] -> Mesh
fromTris !tris = MkMesh $ V.fromList $ map toVecs tris
    where toVecs (Triangle _ (a,b,c)) = (toVec a, toVec b, toVec c)
          toVec (x,y,z) = fromXYZ (float2Double x, float2Double y, float2Double z)
