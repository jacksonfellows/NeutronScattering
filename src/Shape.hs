module Shape
    ( Shape(..)
    , Ray(..)
    , AABB(..)
    , Intersection(..)
    ) where

import           Data.Vec3

import           AABB
import           Ray

class Shape a where
    intersect :: Ray -> a -> Maybe Intersection
    buildAABB :: a -> AABB

data Intersection = MkIntersection
    { getPoint :: CVec3
    , getDist  :: Double
    } deriving (Show, Eq)

instance Ord Intersection where
    (MkIntersection _ a) <= (MkIntersection _ b) = a <= b
