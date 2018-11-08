{-# LANGUAGE NamedFieldPuns #-}

module Scatter
    ( Neutron(..)
    , Object(..)
    , closestIntersection
    , simulate
    , Material(..)
    ) where

import qualified Data.HashTable.IO as H

import Shapes
import Linear
import Volume

-- TODO parametrize these types?

-- is the energy the magnitude of the direction?
data Neutron = Neutron
    { ray :: Ray Double
    , inside :: Maybe Object
    }
    deriving (Show, Read)

data Material = Air | Paraffin
    deriving (Show, Read, Eq, Ord)

-- Units? Real values?
sigmaElasticScattering :: Material -> Double
sigmaElasticScattering Air = 0
sigmaElasticScattering Paraffin = 0.2

sigmaTotal :: Material -> Double
sigmaTotal Air = 0
sigmaTotal Paraffin = 0.1

-- TODO: it doesn't really make sense to "order" objects
data Object = Object
    { shape :: Shape
    , material :: Material
    }
    deriving (Show, Read, Eq, Ord)

-- are we assuming that the neutron is outside of all objects in the scene?
-- returns the intersection along with the object intersected
closestIntersection :: Neutron -> [Object] -> Maybe (Intersection, Object)
closestIntersection Neutron {ray} objs
    | null ints = Nothing
    | otherwise = let (Just int,obj) = minimum ints in Just (int,obj)
    where ints = filter (\(int,_) -> int /= Nothing) $ zip (map (\Object {shape} -> intersection ray shape) objs) objs


-- takes a map to update
-- along with a neutron source and a scene (list of Objects)
simulate :: HashTable (Int, Int, Int) Float -> V3 Double -> [Object] -> IO ()
simulate intensities source scene = do
    dir <- randomDir
    let n = Neutron $ Ray source dir
    simulate' intensities n scene

-- TODO: ugly
toKey :: V3 Double -> (Int, Int, Int)
toKey (V3 x y z) = (floor x, floor y, floor z)

-- collide a neutron with the scene
collideOutside :: HashTable (Int, Int, Int) -> Neutron -> [Object] -> IO (Maybe newN)
collideOutside intensities n scene = do
    let intPair = closestIntersection n scene
    if intPair == Nothing
    then do return ()
    else do
        let Just (int,obj) = intPair
        collide

-- collide a neutron with the object it is inside
collideInside :: HashTable (Int, Int, Int) -> Neutron -> IO (Maybe newN)

-- simulate starting with a neutron
simulate' :: HashTable (Int, Int, Int) Float -> Neutron -> [Object] -> IO ()
simulate' intensities n scene = do
    -- TODO: very ugly
    if (inside n) == Nothing
    then do
        newN <- collideOutside intensities n scene
    else do
        newN <- collideInside intensities n

    if newN == Nothing
    then do return ()
    else do simulate' intensities newN scene
