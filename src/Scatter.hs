{-# LANGUAGE NamedFieldPuns #-}

module Scatter
    ( Neutron(..)
    , Material(..)
    , Object(..)
    , simulate
    , SimState(..)
    -- for debugging
    , getIntersection
    , closestIntersection
    ) where

import           Codec.Picture.Types
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.STRef
import           Data.Vec3
import           System.Random.MWC         as MWC

import           Object
import           Shape

_air_ = MkMat { getSigmaScat = const 0, getSigmaTot = const 0, getName = "air" }

data Neutron = MkNeutron
    { ray    :: Ray
    , inside :: Maybe Object
    } deriving (Show)

randomDir :: MWC.GenST s -> ST s CVec3
randomDir gen = do
    r0 <- MWC.uniform gen
    r1 <- MWC.uniform gen
    let theta = r0 * 2 * pi
        phi = acos $ r1 * 2 - 1
    return $ CVec3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

closestIntersection :: Neutron -> [Object] -> Maybe (Intersection, Object)
closestIntersection MkNeutron {ray} objs
    | null ints = Nothing
    | otherwise = let (Just int,obj) = minimum ints in Just (int,obj)
    where ints = filter (\(int,_) -> int /= Nothing) $ zip (map (\MkObject {getShape=shape} -> ray `intersect` shape) objs) objs

getIntersection :: Neutron -> [Object] -> Maybe (Intersection, Object)
getIntersection n@(MkNeutron {ray, inside}) objs
    | inside == Nothing = closestIntersection n objs -- outside of all objects
    | otherwise = do
        obj <- inside
        int <- ray `intersect` (getShape obj)
        return (int,obj)

data SimState s = MkSimState
    { getGen        :: MWC.GenST s
    , getAdder      :: CVec3 -> Pixel8 -> ST s ()
    , numCollisions :: STRef s Int
    }

-- assuming that we are starting outside of all the objects in the scene
simulate :: SimState s
         -> CVec3 -- source
         -> [Object] -- scene
         -> ST s () -- updates to the image
simulate state@(MkSimState gen _ _) source scene = do
    dir <- randomDir gen
    -- let n = MkNeutron {ray = MkRay source dir, inside = Nothing}
    -- TODO: check if we are inside an object
    -- I'm not implementing this right now because we might not need it
    -- For the bunny tests that I am doing, we always start inside the bunny,
    -- which is the only item in the scene
    let n = MkNeutron {ray = MkRay source dir, inside = Just $ head scene}

    -- TODO: ugly
    _ <- runMaybeT $ simulate' state n scene
    return ()

-- ugly helper
pointOnRay :: Ray -> Double -> CVec3
pointOnRay (MkRay o d) n = o <+> (d .^ n)

-- the neutron can start anywhere
simulate' :: SimState s
          -> Neutron -- neutron
          -> [Object] -- scene
          -> MaybeT (ST s) () -- (possible) updates to the image
simulate' state@(MkSimState gen addToImage numCols) n scene = do
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
        lift $ modifySTRef numCols (+1)

        let colPoint = pointOnRay (ray n) distanceCovered
        r1 <- uniform gen

        if (sigmaScat / sigmaTot) > r1
        -- scattering
        then do
            lift $ addToImage colPoint 25 -- TODO: actual intensity

            newDir <- lift $ randomDir gen -- TODO: actual new direction
            let newN = MkNeutron {ray = MkRay colPoint newDir, inside = (inside n)}

            simulate' state newN scene
        -- absorbed
        else do
            lift $ addToImage colPoint 25 -- TODO: actual intensity
    -- move into next object
    else do
        -- TODO: replace with global epsilon?
        let newP = pointOnRay (MkRay (getPoint int) (getDir (ray n))) 1e-3
            newN = MkNeutron {ray = MkRay (newP) (getDir (ray n)), inside = Just obj}
        simulate' state newN scene
