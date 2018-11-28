module AABB
    ( AABB(..)
    , intersects
    ) where

import           Data.Vec3

import           Ray

data AABB = MkAABB
    { getMin :: CVec3
    , getMax :: CVec3
    } deriving (Show)

intersects :: Ray -> AABB -> Bool
(MkRay o d) `intersects` (MkAABB b0 b1)
    | tmin > tmax = False
    | tmin > 0 || tmax > 0 = True
    | otherwise = False
    where (tmin, tmax) = foldl f ((-1)/0, 1/0) $ zip (ints b0) (ints b1)
          f (tmin, tmax) (t0, t1) = (max tmin (min t0 t1), min tmax (max t0 t1))
          ints b = [x,y,z]
              where (x,y,z) = toXYZ i
                    i = Data.Vec3.zipWith (/) (b <-> o) d
