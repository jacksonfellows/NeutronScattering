module AABB
    ( AABB(..)
    ) where

import           Data.Vec3

import           Ray

data AABB = MkAABB
    { getMin :: CVec3
    , getMax :: CVec3
    } deriving (Show)

intersects :: Ray -> AABB -> Bool
(MkRay o d) `intersects` (MkAABB b0 b1)
    | t0 > t1 = False
    | t0 > 0 || t1 > 0 = True
    | otherwise = False
    where t0 = maximum $ ints b1
          t1 = minimum $ ints b1
          ints b = [x,y,z]
              where (x,y,z) = toXYZ i
                    i = Data.Vec3.zipWith (/) (b <-> o) d
