{-# LANGUAGE NamedFieldPuns #-}

module Scatter
    ( Neutron(..)
    , Object(..)
    , closestIntersection
    , simulate
    ) where

import Shapes
import Linear
import Volume

-- TODO parametrize these types?

-- is the energy the magnitude of the direction?
data Neutron = Neutron
    { ray :: Ray Double }
    deriving (Show, Read)

-- TODO: it doesn't really make sense to "order" objects
data Object = Object
    { shape :: Shape
    , name :: String -- test
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
