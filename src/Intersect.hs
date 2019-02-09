{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}

module Intersect
     ( Intersectable(..)
     , IntersectableScene(..)
     , intersectAny
     , buildAABBAny
     , getCentroidAny
     , intersectScene
     , buildAABBScene
     , IntersectionPrim(..)
     , AccelerationStructure(..)
     ) where

import           AABB
import           Data.Vec3
import           Ray

-- some "boxes" to represent any intersection primitive and any acceleration structure
data Intersectable = forall i. IntersectionPrim i => AnyIntersectable i
data IntersectableScene i = forall a. AccelerationStructure a i => AnyIntersectableScene (a i)

ray `intersectAny` (AnyIntersectable i) = ray `intersectPrim` i
buildAABBAny (AnyIntersectable i) = buildAABBPrim i
getCentroidAny (AnyIntersectable i) = getCentroidPrim i

ray `intersectScene` (AnyIntersectableScene s) = ray `intersect` s
buildAABBScene (AnyIntersectableScene s) = buildAABB s

class IntersectionPrim a where
    intersectPrim :: Ray -> a -> Maybe Double
    buildAABBPrim :: a -> AABB
    getCentroidPrim :: a -> CVec3

class IntersectionPrim i => AccelerationStructure a i where
    intersect :: Ray -> a i -> Maybe (Double, i)
    construct :: [i] -> a i
    buildAABB :: a i -> AABB

instance (IntersectionPrim i, AccelerationStructure a i) => IntersectionPrim (a i) where
    intersectPrim ray struct = fst <$> intersect ray struct
    buildAABBPrim = buildAABB
    getCentroidPrim struct = let box = buildAABB struct in (getMin box <+> getMax box) .^ (1/2)
