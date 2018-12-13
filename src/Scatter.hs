{-# LANGUAGE NamedFieldPuns #-}

module Scatter
    ( Neutron(..)
    , Material(..)
    , Object(..)
    , simulate
    ) where

import           Codec.Picture.Types
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Vec3
import           System.Random.MWC         as MWC

import           BVH
import           Object
import           Shape

_air_ = MkMat { getSigmaScat = const 0, getSigmaTot = const 0, getName = "air" }

data Neutron a = MkNeutron
    { ray    :: Ray
    , inside :: Maybe (Object a)
    } deriving (Show)

randomDir :: MWC.GenST s -> ST s CVec3
randomDir gen = do
    r0 <- MWC.uniform gen
    r1 <- MWC.uniform gen
    let theta = r0 * 2 * pi
        phi = acos $ r1 * 2 - 1
    return $ CVec3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

getIntersection :: (Shape a) => Neutron a -> BVHTree a -> Maybe (Intersection, Object a)
getIntersection n@(MkNeutron {ray, inside}) scene
    | inside == Nothing = ray `intersectBVH` scene -- outside of all objects
    | otherwise = do
        obj <- inside
        int <- ray `intersect` (getShape obj)
        return (int,obj)

-- assuming that we are starting outside of all the objects in the scene
simulate :: (Shape a, Show a)
         => GenST s -- random number generator
         -> MutableImage s Pixel8
         -> CVec3 -- source
         -> BVHTree a -- scene
         -> ST s () -- updates to the image
simulate gen image source scene = do
    dir <- randomDir gen
    let n = MkNeutron {ray = MkRay source dir, inside = Nothing}

    -- TODO: ugly
    nil <- runMaybeT $ simulate' gen image n scene
    return ()

-- ugly helper
pointOnRay :: Ray -> Double -> CVec3
pointOnRay (MkRay o d) n = o <+> (d .^ n)

-- the neutron can start anywhere
simulate' :: (Shape a, Show a)
          => MWC.GenST s -- random number generator
          -> MutableImage s Pixel8
          -> Neutron a -- neutron
          -> BVHTree a -- scene
          -> MaybeT (ST s) () -- (possible) updates to the image
simulate' gen image n scene = do
    -- find the intersection and object intersected
    (int,obj) <- MaybeT . return $ getIntersection n scene

    -- TODO: god-awful
    let mat = let o = inside n in if o == Nothing then _air_ else let (Just ob) = o in getMat ob

    -- get the collision based on the intersection and the material
    let sigmaScat = getSigmaScat mat 1
        sigmaTot = getSigmaTot mat 1

    r0 <- uniform gen
    let distanceCovered = -(1 / sigmaTot) * log r0

    if distanceCovered < getDist int
    -- there was a collision
    then do
        let colPoint = pointOnRay (ray n) distanceCovered
        r1 <- uniform gen

        if (sigmaScat / sigmaTot) > r1
        -- scattering
        then do
            lift $ addToImage image colPoint 25 -- TODO: actual intensity

            newDir <- lift $ randomDir gen -- TODO: actual new direction
            let newN = MkNeutron {ray = MkRay colPoint newDir, inside = (inside n)}

            simulate' gen image newN scene
        -- absorbed
        else do
            lift $ addToImage image colPoint 25 -- TODO: actual intensity
    -- move into next object
    else do
        -- TODO: replace with global epsilon
        let newP = pointOnRay (MkRay (getPoint int) (getDir (ray n))) 0.0001
            newN = MkNeutron {ray = MkRay (newP) (getDir (ray n)), inside = Just obj}
        simulate' gen image newN scene

addToImage :: MutableImage s Pixel8 -> CVec3 -> Pixel8 -> ST s ()
addToImage img pnt = writePixel img iX iY
    where (x,y,z) = toXYZ pnt
          iX = round x
          iY = round (y * z)
