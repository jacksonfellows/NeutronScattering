{-# LANGUAGE NamedFieldPuns #-}

module Scatter
    ( Neutron(..)
    , Material(..)
    , Object(..)
    , simulate
    ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.HashTable.IO         as H
import           Data.Vec3
import           System.Random.MWC         as MWC

import           Object
import           Shape
import           Volume

_air_ = MkMat { getSigmaScat = const 0, getSigmaTot = const 0, getName = "air" }

data Neutron a = MkNeutron
    { ray    :: Ray
    , inside :: Maybe (Object a)
    } deriving (Show)

-- lifts a normal maybe value into MaybeT
liftMaybe = MaybeT . return

-- ugly hack to allow me to sort (Intersection,Object) pairs
-- a runtime error will be thrown if it two objects are actually compared
instance Eq (Object a) where
    _ == _ = undefined

instance Ord (Object a) where
    _ <= _ = undefined

-- move to ST monad?
randomDir :: MWC.GenIO -> IO CVec3
randomDir gen = do
    r0 <- MWC.uniform gen
    r1 <- MWC.uniform gen
    let theta = r0 * 2 * pi
        phi = acos $ r1 * 2 - 1
    return $ CVec3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

-- TODO: looks god-awful
-- TODO: rewrite using Maybe monad?
-- returns the intersection along with the object intersected
closestIntersection :: (Shape a) => Neutron a -> [Object a] -> Maybe (Intersection, Object a)
closestIntersection MkNeutron {ray} objs
    | null ints = Nothing
    | otherwise = let (Just int,obj) = minimum ints in Just (int,obj)
        where ints = filter (\(int,_) -> int /= Nothing) $ zip (map (\MkObject {getShape=shape} -> ray `intersect` shape) objs) objs

getIntersection :: (Shape a) => Neutron a -> [Object a] -> Maybe (Intersection, Object a)
getIntersection n@(MkNeutron {ray, inside}) objs
    | inside == Nothing = closestIntersection n objs -- outside of all objects
    | otherwise = do
        obj <- inside
        int <- ray `intersect` (getShape obj)
        return (int,obj)

-- assuming that we are starting outside of all the objects in the scene
simulate :: (Shape a) =>
         MWC.GenIO -- random number generator
         -> HashTable (Int, Int, Int) Float -- map to update
         -> CVec3 -- source
         -> [Object a] -- scene
         -> IO () -- updates to the map
simulate gen intensities source scene = do
    dir <- randomDir gen
    let n = MkNeutron {ray = MkRay source dir, inside = Nothing}

    -- TODO: ugly
    -- putStrLn "neutron:"
    nil <- runMaybeT $ simulate' gen intensities n scene
    return ()

-- ugly helper
pointOnRay :: Ray -> Double -> CVec3
pointOnRay (MkRay o d) n = o <+> (d .^ n)

-- the neutron can start anywhere
simulate' :: (Shape a)
          => MWC.GenIO -- random number generator
          -> HashTable (Int, Int, Int) Float -- map to update
          -> Neutron a -- neutron
          -> [Object a] -- scene
          -> MaybeT IO () -- (possible) updates to the
simulate' gen intensities n scene = do
    -- find the intersection and object intersected
    (int,obj) <- liftMaybe $ getIntersection n scene

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
        -- lift $ putStrLn $ "collision in " ++ show mat
        -- lift $ putStrLn $ "distanceCovered: " ++ show distanceCovered
        let colPoint = pointOnRay (ray n) distanceCovered
        r1 <- uniform gen

        if (sigmaScat / sigmaTot) > r1
        -- scattering
        then do
            -- lift $ putStrLn "- scattering"
            lift $ addToVolume intensities colPoint 0.1 -- TODO: actual intensity

            newDir <- lift $ randomDir gen -- TODO: actual new direction
            let newN = MkNeutron {ray = MkRay colPoint newDir, inside = (inside n)}

            simulate' gen intensities newN scene
        -- absorbed
        else do
            -- lift $ putStrLn "- absorbed"
            lift $ addToVolume intensities colPoint 0.1 -- TODO: actual intensity
    -- move into next object
    else do
        -- lift $ putStrLn "moving into next object"
        -- TODO: replace with global epsilon
        let newP = pointOnRay (MkRay (getPoint int) (getDir (ray n))) 0.0001
            newN = MkNeutron {ray = MkRay (newP) (getDir (ray n)), inside = Just obj}
        simulate' gen intensities newN scene
