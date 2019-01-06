{-# LANGUAGE ExistentialQuantification #-}

module AccelerationStructure
    ( Intersectable(..)
    , AccelerationStructure(..)
    , getIntersection
    , Intersection(..)
    ) where

import           Data.Vec3

import           AABB
import           Ray
import           Triangle

data Intersectable = forall a. AccelerationStructure a => AnyIntersectable a

getIntersection :: Ray -> Intersectable -> Maybe Intersection
ray `getIntersection` (AnyIntersectable struct) = ray `intersect` struct

class AccelerationStructure a where
    intersect :: Ray -> a -> Maybe Intersection
    build :: [Triangle] -> a

data Intersection = MkIntersection
    { getPoint :: CVec3
    , getDist  :: Double
    } deriving (Show, Eq)

instance Ord Intersection where
    (MkIntersection _ a) <= (MkIntersection _ b) = a <= b
