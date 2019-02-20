{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module AABB
    ( AABB
    , aabb
    , getMin
    , getMax
    , intersects
    , intersectTs
    , union
    , contains
    , containsPoint
    , midpoint
    ) where

import           Control.Monad.Zip
import           Data.Vector.Unboxed.Deriving
import           Linear.V3

import           Ray

data AABB = MkAABB
    { getMin :: V3 Double
    , getMax :: V3 Double
    } deriving (Show, Eq)

aabb :: V3 Double -> V3 Double -> AABB
aabb = MkAABB

derivingUnbox "AABB"
    [t| AABB -> (V3 Double, V3 Double) |]
    [| \ (MkAABB minBounds maxBounds) -> (minBounds, maxBounds) |]
    [| \ (minBounds, maxBounds) -> MkAABB minBounds maxBounds |]

intersects :: Ray -> AABB -> Bool
intersects ray box = tmax >= tmin && (tmax > 0 || tmin > 0)
    where (tmin,tmax) = intersectTs ray box

intersectTs :: Ray -> AABB -> (Double, Double)
(MkRay o _ inv) `intersectTs` (MkAABB b0 b1) = (tmin,tmax)
    where mins = (b0 - o) * inv
          maxs = (b1 - o) * inv
          tmin = maximum $ mzipWith min mins maxs
          tmax = minimum $ mzipWith max mins maxs

union :: AABB -> AABB -> AABB
union (MkAABB min0 max0) (MkAABB min1 max1) = MkAABB newMin newMax
    where newMin = mzipWith min min0 min1
          newMax = mzipWith max max0 max1

contains :: AABB -> AABB -> Bool
a `contains` b = a == union a b

containsPoint :: AABB -> V3 Double -> Bool
(MkAABB (V3 minX minY minZ) (V3 maxX maxY maxZ)) `containsPoint` (V3 x y z) =
    minX < x && x < maxX && minY < y && y < maxY && minZ < z && z < maxZ

midpoint :: AABB -> V3 Double
midpoint (MkAABB min max) = (min + max) / 2
