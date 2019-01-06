{-# LANGUAGE ExistentialQuantification #-}

module Shape
    ( Shape(..)
    , intersect
    , IntersectionPrimitive(..)
    , Ray(..)
    , AABB(..)
    , Intersection(..)
    ) where

import           Data.Vec3

import           AABB
import           Ray

data Shape = forall a. IntersectionPrimitive a => AnyShape a

intersect :: Ray -> Shape -> Maybe Intersection
ray `intersect` (AnyShape shape) = ray `intersectPrim` shape

class IntersectionPrimitive a where
    intersectPrim :: Ray -> a -> Maybe Intersection
    buildAABBPrim :: a -> AABB

data Intersection = MkIntersection
    { getPoint :: CVec3
    , getDist  :: Double
    } deriving (Show, Eq)

instance Ord Intersection where
    (MkIntersection _ a) <= (MkIntersection _ b) = a <= b
