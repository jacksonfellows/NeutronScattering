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

import           Linear
import           Shapes
import           Types
import           Volume

-- lifts a normal maybe value into MaybeT
liftMaybe = MaybeT . return

-- TODO: parametrize these types?
-- TODO: move all types to separate file?

-- is the energy the magnitude of the direction?
data Neutron = Neutron
    { ray    :: Ray Double
    , inside :: Maybe Object
    }
    deriving (Show, Read)

data Material = Air | Paraffin
    deriving (Show, Read, Eq, Ord)

-- Units? Real values?
sigmaElasticScattering :: Material -> Double
sigmaElasticScattering Air      = 0
sigmaElasticScattering Paraffin = 0.2

sigmaTotal :: Material -> Double
sigmaTotal Air      = 0
sigmaTotal Paraffin = 0.1

-- TODO: it doesn't really make sense to "order" objects
data Object = Object
    { shape    :: Shape
    , material :: Material
    }
    deriving (Show, Read, Eq, Ord)

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
         -> V3 Double -- source
         -> [Object] -- scene
         -> IO () -- updates to the map
simulate intensities source scene = do
    dir <- randomDir
    let n = Neutron {ray = Ray source dir, inside = Nothing}

    num <- runMaybeT $ simulate' intensities n scene
    case num of
        Nothing -> return () -- print "no collisions :("
        Just n  -> print $ "had " ++ (show n) ++ " collision(s) :)"

-- TODO: return MaybeT IO ()? Would this allow us to use Maybe monad to skip no intersections?
-- the neutron can start anywhere
simulate' :: HashTable (Int, Int, Int) Float -- map to update
          -> Neutron -- neutron
          -> [Object] -- scene
          -> MaybeT IO Int -- (possible) updates to the map along with number of collisions (or steps)
simulate' intensities n scene = do
    -- find the intersection and object intersected
    (int,obj) <- liftMaybe $ getIntersection n scene

    -- get the collision based on the intersection and the material

    lift $ print "hit something!"
    lift $ H.insert intensities (toKey (point int)) 0.5

    return 1

-- convert a point to a key
toKey :: V3 Double -> (Int, Int, Int)
toKey v = let (V3 x y z) = fmap floor v in (x, y, z)
