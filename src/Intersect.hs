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
import           Linear.V3
import           Ray

-- some "boxes" to represent any intersection primitive and any acceleration structure
data Intersectable n = forall i. IntersectionPrim i n => AnyIntersectable (i n)
data IntersectableScene n i = forall a. AccelerationStructure a i n => AnyIntersectableScene (a i n)

ray `intersectAny` (AnyIntersectable i) = ray `intersectPrim` i
buildAABBAny (AnyIntersectable i) = buildAABBPrim i
getCentroidAny (AnyIntersectable i) = getCentroidPrim i

ray `intersectScene` (AnyIntersectableScene s) = ray `intersect` s
buildAABBScene (AnyIntersectableScene s) = buildAABB s

class IntersectionPrim a n where
    intersectPrim :: (Fractional n, Ord n) => Ray n -> a n -> Maybe n
    buildAABBPrim :: (Fractional n, Ord n) => a n -> AABB n
    getCentroidPrim :: (Fractional n, Ord n) => a n -> V3 n

class IntersectionPrim i n => AccelerationStructure a i n where
    intersect :: (Fractional n, Ord n) => Ray n -> a i n -> Maybe (n, i n)
    construct :: (Fractional n, Ord n) => [i n] -> a i n
    buildAABB :: (Fractional n, Ord n) => a i n -> AABB n

instance (IntersectionPrim i n, AccelerationStructure a i n) => IntersectionPrim (a i) n where
    intersectPrim ray struct = fst <$> intersect ray struct
    buildAABBPrim = buildAABB
    getCentroidPrim struct = let box = buildAABB struct in midpoint box
