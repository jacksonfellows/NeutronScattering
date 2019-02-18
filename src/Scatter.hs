{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Scatter
    ( Neutron(..)
    , Material(..)
    , Object(..)
    , simulate
    , SimState(..)
    , emptyStats
    , freezeStats
    , FrozenStats(..)
    ) where

import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.STRef
import           Linear.V3
import           System.Random.MWC         as MWC

import           Intersect
import           Object
import           Ray
import           Slices

_air_ :: Material
_air_ = MkMat { getSigmaScat = const 0, getSigmaTot = const 0, getName = "air" }

data Neutron = MkNeutron
    { ray    :: Ray
    , inside :: Maybe Object
    } deriving (Show)

randomDir :: MWC.GenST s -> ST s (V3 Double)
randomDir gen = do
    r0 <- MWC.uniform gen
    r1 <- MWC.uniform gen
    let theta = r0 * 2 * pi
        phi = acos $ r1 * 2 - 1
    return $ V3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

data SimState s = MkSimState
    { getGen   :: MWC.GenST s
    , getAdder :: Adder s
    , getStats :: Stats s
    }

data Stats s = MkStats
    { numScatteredRef, numAbsorbedRef :: STRef s Int }

incNumScattered, incNumAbsorbed :: Stats s -> ST s ()
incNumScattered stats = modifySTRef' (numScatteredRef stats) (+1)
incNumAbsorbed stats = modifySTRef' (numAbsorbedRef stats) (+1)

emptyStats :: ST s (Stats s)
emptyStats = do
    scattered <- newSTRef 0
    absorbed <- newSTRef 0
    return $ MkStats scattered absorbed

data FrozenStats = MkFrozenStats
    { getNumScattered, getNumAbsorbed :: !Int }

freezeStats :: Stats s -> ST s FrozenStats
freezeStats MkStats {..} = do
    scattered <- readSTRef numScatteredRef
    absorbed <- readSTRef numAbsorbedRef
    return $ MkFrozenStats scattered absorbed

-- ugly helper
pointOnRay :: Ray -> Double -> V3 Double
pointOnRay (MkRay o d _) n = o - (d * (pure n))

-- assuming that we are starting outside of all the objects in the scene
simulate :: SimState s
         -> V3 Double -- source
         -> Maybe Object -- object the neutron starts inside
         -> IntersectableScene Object -- scene
         -> ST s () -- updates to the image
simulate state@(MkSimState gen addToImage stats) source initialInside scene = do
    dir <- randomDir gen
    -- let n = MkNeutron {ray = MkRay source dir, inside = Nothing}
    -- TODO: check if we are inside an object
    -- I'm not implementing this right now because we might not need it
    -- For the bunny tests that I am doing, we always start inside the bunny,
    -- which is the only item in the scene
    let n = MkNeutron {ray = buildRay source dir, inside = initialInside}

    -- TODO: ugly
    _ <- runMaybeT $ go state n scene
    return ()
    where go state n scene = do
              -- the neutron can start anywhere
              -- find the intersection and object intersected
              (dist,obj) <- MaybeT . return $ case inside n of
                  Nothing  -> (ray n) `intersectScene` scene
                  Just obj -> (,) <$> (ray n) `intersectPrim` obj <*> return obj

              -- get the collision based on the CURRENT intersection and the material
              let mat = case inside n of
                      Just o  -> getMat o
                      Nothing -> _air_
                  sigmaScat = getSigmaScat mat 1
                  sigmaTot = getSigmaTot mat 1

              r0 <- uniform gen
              let distanceCovered = -(1 / sigmaTot) * log r0

              if distanceCovered < dist
              -- there was a collision
              then do
                  let colPoint = pointOnRay (ray n) distanceCovered
                  r1 <- uniform gen

                  if (sigmaScat / sigmaTot) > r1
                  -- scattering
                  then do
                      lift $ incNumScattered stats
                      lift $ addToImage colPoint 25 -- TODO: actual intensity

                      newDir <- lift $ randomDir gen -- TODO: actual new direction
                      let newN = MkNeutron {ray = buildRay colPoint newDir, inside = (inside n)}

                      go state newN scene
                  -- absorbed
                  else do
                      lift $ incNumAbsorbed stats
                      lift $ addToImage colPoint 25 -- TODO: actual intensity
              -- move into next object
              else do
                  -- TODO: replace with global epsilon?
                  let newP = pointOnRay (ray n) (dist + 1e-3)
                      newN = MkNeutron {ray = buildRay (newP) (getDir (ray n)), inside = Just obj}
                  go state newN scene
