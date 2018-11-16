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
import           System.Random             (randomRIO)

import           Shapes
import           Types
import           Volume

-- lifts a normal maybe value into MaybeT
liftMaybe = MaybeT . return

-- Units? Real values?
getSigmaElasticScattering :: Material -> Double
getSigmaElasticScattering Air      = 0
getSigmaElasticScattering Paraffin = 0.2

getSigmaTotal :: Material -> Double
getSigmaTotal Air      = 0
getSigmaTotal Paraffin = 1

-- TODO: looks god-awful
-- TODO: rewrite using Maybe monad?
-- returns the intersection along with the object intersected
closestIntersection :: Neutron -> [Object] -> Maybe (Intersection, Object)
closestIntersection Neutron {ray} objs
    | null ints = Nothing
    | otherwise = let (Just int,obj) = minimum ints in Just (int,obj)
        where ints = filter (\(int,_) -> int /= Nothing) $ zip (map (\Object {shape} -> intersection ray shape) objs) objs

getIntersection :: Neutron -> [Object] -> Maybe (Intersection, Object)
getIntersection n@(Neutron {ray, inside}) objs
    | inside == Nothing = closestIntersection n objs -- outside of all objects
    | otherwise = do
        obj <- inside
        int <- intersection ray (shape obj)
        return (int,obj)

-- assuming that we are starting outside of all the objects in the scene
simulate :: HashTable (Int, Int, Int) Float -- map to update
         -> CVec3 -- source
         -> [Object] -- scene
         -> IO () -- updates to the map
simulate intensities source scene = do
    dir <- randomDir
    let n = Neutron {ray = Ray source dir, inside = Nothing}

    -- TODO: ugly
    -- putStrLn "neutron:"
    nil <- runMaybeT $ simulate' intensities n scene
    return ()

-- the neutron can start anywhere
simulate' :: HashTable (Int, Int, Int) Float -- map to update
          -> Neutron -- neutron
          -> [Object] -- scene
          -> MaybeT IO () -- (possible) updates to the
simulate' intensities n scene = do
    -- find the intersection and object intersected
    (int,obj) <- liftMaybe $ getIntersection n scene

    -- TODO: god-awful
    let mat = let o = inside n in if o == Nothing then Air else let (Just ob) = o in material ob

    -- get the collision based on the intersection and the material
    let sigmaScat = getSigmaElasticScattering mat
        sigmaTot = getSigmaTotal mat

    r0 <- lift $ randomRIO (0,1) -- between 0 and 1?
    let distanceCovered = -(1 / sigmaTot) * log r0

    if distanceCovered < (distanceFrom int)
    -- there was a collision
    then do
        -- lift $ putStrLn $ "collision in " ++ show mat
        -- lift $ putStrLn $ "distanceCovered: " ++ show distanceCovered
        let colPoint = pointOnRay (ray n) distanceCovered
        r1 <- lift $ randomRIO (0,1) -- between 0 and 1?

        if (sigmaScat / sigmaTot) > r1
        -- scattering
        then do
            -- lift $ putStrLn "- scattering"
            lift $ addToVolume intensities colPoint 0.1 -- TODO: actual intensity

            newDir <- lift $ randomDir -- TODO: actual new direction
            let newN = Neutron {ray = Ray colPoint newDir, inside = (inside n)}

            simulate' intensities newN scene
        -- absorbed
        else do
            -- lift $ putStrLn "- absorbed"
            lift $ addToVolume intensities colPoint 0.1 -- TODO: actual intensity
    -- move into next object
    else do
        -- lift $ putStrLn "moving into next object"
        -- TODO: replace with global epsilon
        let newP = pointOnRay (Ray (point int) (dir (ray n))) 0.0001
            newN = Neutron {ray = Ray (newP) (dir (ray n)), inside = Just obj}
        simulate' intensities newN scene
