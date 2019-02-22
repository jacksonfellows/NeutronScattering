{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module AABB
    ( AABB
    , aabb
    , getMin
    , getMax
    , intersects
    , union
    , contains
    , containsPoint
    , midpoint
    ) where

import           Control.Monad.Zip
import           Data.Vector.Unboxed          (Unbox)
import           Data.Vector.Unboxed.Deriving
import           Linear.V3

import           Ray

data AABB a = MkAABB
    { getMin :: V3 a
    , getMax :: V3 a
    } deriving (Show, Eq)

aabb = MkAABB

derivingUnbox "AABB"
    [t| forall a. (Unbox a) => AABB a -> (V3 a, V3 a) |]
    [| \ (MkAABB minBounds maxBounds) -> (minBounds, maxBounds) |]
    [| \ (minBounds, maxBounds) -> MkAABB minBounds maxBounds |]

intersects :: (Num a, Ord a) => Ray a -> AABB a -> Bool
(MkRay o _ inv) `intersects` (MkAABB b0 b1) = tmax >= tmin && (tmax > 0 || tmin > 0)
    where mins = (b0 - o) * inv
          maxs = (b1 - o) * inv
          tmin = maximum $ mzipWith min mins maxs
          tmax = minimum $ mzipWith max mins maxs

union :: (Ord a) => AABB a -> AABB a -> AABB a
union (MkAABB min0 max0) (MkAABB min1 max1) = MkAABB newMin newMax
    where newMin = mzipWith min min0 min1
          newMax = mzipWith max max0 max1

contains :: (Eq a, Ord a) => AABB a -> AABB a -> Bool
a `contains` b = a == union a b

containsPoint :: (Ord a) => AABB a -> V3 a -> Bool
(MkAABB (V3 minX minY minZ) (V3 maxX maxY maxZ)) `containsPoint` (V3 x y z) =
    minX < x && x < maxX && minY < y && y < maxY && minZ < z && z < maxZ

midpoint :: (Fractional a) => AABB a -> V3 a
midpoint (MkAABB min max) = (min + max) / 2
