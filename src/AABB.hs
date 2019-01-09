{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module AABB
    ( AABB(..)
    , intersects
    , union
    , contains
    , containsPoint
    , containsTri
    , aabbFromTri
    ) where

import           Data.Vec3
import           Data.Vector.Unboxed.Deriving
import           Prelude                      hiding (zipWith)

import           Ray

data AABB = MkAABB
    { getMin :: CVec3
    , getMax :: CVec3
    } deriving (Show, Eq)

derivingUnbox "AABB"
    [t| AABB -> (CVec3, CVec3) |]
    [| \ (MkAABB minBounds maxBounds) -> (minBounds, maxBounds) |]
    [| \ (minBounds, maxBounds) -> MkAABB minBounds maxBounds |]

intersects :: Ray -> AABB -> Bool
(MkRay o d) `intersects` (MkAABB b0 b1)
    | tmin > tmax = False
    | tmin > 0 || tmax > 0 = True
    | otherwise = False
    where (tmin, tmax) = foldl f ((-1)/0, 1/0) $ zip (ints b0) (ints b1)
          f (tmin, tmax) (t0, t1) = (max tmin (min t0 t1), min tmax (max t0 t1))
          ints b = [x,y,z]
              where (x,y,z) = toXYZ i
                    i = zipWith (/) (b <-> o) d

union :: AABB -> AABB -> AABB
union (MkAABB min0 max0) (MkAABB min1 max1) = MkAABB newMin newMax
    where newMin = zipWith min min0 min1
          newMax = zipWith max max0 max1

contains :: AABB -> AABB -> Bool
a `contains` b = a == union a b

containsPoint :: AABB -> CVec3 -> Bool
(MkAABB (CVec3 minX minY minZ) (CVec3 maxX maxY maxZ)) `containsPoint` (CVec3 x y z) =
    minX < x && x < maxX && minY < y && y < maxY && minZ < z && z < maxZ

containsTri :: AABB -> (CVec3,CVec3,CVec3) -> Bool
containsTri aabb (a,b,c) = aabb `containsPoint` a && aabb `containsPoint` b && aabb `containsPoint` c

-- TODO: ugly
aabbFromTri :: (CVec3,CVec3,CVec3) -> AABB
aabbFromTri (a,b,c) = MkAABB (zipMin a (zipMin b c) <-> eps) (zipMax a (zipMax b c) <+> eps)
    where zipMin = zipWith min
          zipMax = zipWith max
          eps = CVec3 1e-3 1e-3 1e-3
