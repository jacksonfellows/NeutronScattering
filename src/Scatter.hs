{-# LANGUAGE NamedFieldPuns #-}

module Scatter
    ( Neutron(..)
    , Object(..)
    , closestIntersection
    , simulate
    , Material(..)
    ) where

import Shapes
import Linear
import Volume

-- TODO parametrize these types?

-- is the energy the magnitude of the direction?
data Neutron = Neutron
    { ray :: Ray Double }
    deriving (Show, Read)

data Material = Air | Paraffin
    deriving (Show, Read, Eq, Ord)

-- 
-- -- Units?
-- sigmaElasticScattering :: Material -> Double
-- sigmaElasticScattering Air = 0.1
-- sigmaElasticScattering Paraffin = 0.1
-- 
-- sigmaTotal :: Material -> Double
-- sigmaTotal Air = 0.1
-- sigmaTotal Paraffin = 0.1

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


-- takes a neutron source and a scene (list of Objects)
-- return a list of points and intensities, representing the path of a neutron
-- TODO: should it update some sort of globally mutable state instead?
simulate :: V3 Double -> [Object] -> IO [(V3 Double, Float)]
simulate source scene = do
    dir <- randomDir
    let n = Neutron $ Ray source dir
        int = closestIntersection n scene
        points = if int == Nothing
                 then []
                 else
                    let Just ((Intersection pos _),_) = int in
                    [(pos, 1)]
    return points
