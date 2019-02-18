{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Triangle
    ( Triangle
    , tri
    , (<++>)
    ) where

import           Control.Monad.Zip
import           Linear.Metric         (dot)
import           Linear.V3
import           Prelude               hiding (zipWith)
import           Test.Tasty.QuickCheck (Arbitrary)

import           AABB
import           ArbitraryHelpers
import           Intersect
import           Ray

newtype Triangle = MkTri (V3 Double,V3 Double,V3 Double)
    deriving (Arbitrary, Show)

tri :: (V3 Double,V3 Double,V3 Double) -> Triangle
tri = MkTri

(<++>) :: Triangle -> V3 Double -> Triangle
MkTri (a, b, c) <++> offset = tri (a + offset, b + offset, c + offset)

instance IntersectionPrim Triangle where
    -- moller-trumbore algorithm (copied from internet)
    -- TODO: make it more haskelly (or, make it less haskelly be removing Maybe?)
    -- TODO: for now, no culling -> make this dependent on whether I'm inside or outside an object?
    (MkRay o d _) `intersectPrim` (MkTri (v0,v1,v2))
        | abs det < 1e-5 = Nothing
        | u < 0 || u > 1 = Nothing
        | v < 0 || u + v > 1 = Nothing
        | t < 0 = Nothing
        | otherwise = Just t
        where v0v1 = v1 - v0
              v0v2 = v2 - v0
              pvec = d `cross` v0v2
              det = v0v1 `dot` pvec
              invDet = 1 / det
              tvec = o - v0
              u = (tvec `dot` pvec) * invDet
              qvec = tvec `cross` v0v1
              v = (d `dot` qvec) * invDet
              t = (v0v2 `dot` qvec) * invDet

    buildAABBPrim (MkTri (a,b,c)) = aabb (zipMin a (zipMin b c) - eps) (zipMax a (zipMax b c) + eps)
        where zipMin = mzipWith min
              zipMax = mzipWith max
              eps = V3 1e-3 1e-3 1e-3

    getCentroidPrim (MkTri (a,b,c)) = (a + b + c) / 3

