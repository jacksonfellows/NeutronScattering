module Main where

import           Control.Monad     (replicateM_)
import qualified Data.HashTable.IO as H
import           Data.Vec3
import qualified Data.Vector
import           System.Random.MWC as MWC

import           Scatter
import           Shapes
import           Types
import           Volume

source = CVec3 50 50 50
-- scene =
--     [ Object { shape = Sphere (CVec3 50 50 70) 10
--              , material = Paraffin
--              }
--     , Object { shape = Sphere (CVec3 50 50 30) 10
--              , material = Paraffin
--              }
--     ]

-- helper function to generate random scenes for testing
randomSpheres :: GenIO -> Int -> Double -> Double-> IO [Shape]
randomSpheres gen n pntScale radScale = do
    xs <- MWC.uniformVector gen n
    ys <- MWC.uniformVector gen n
    zs <- MWC.uniformVector gen n
    let points = Data.Vector.map fromXYZ $ Data.Vector.zip3 xs ys zs

    radii <- MWC.uniformVector gen n

    let scaledPnts = Data.Vector.map (.^ pntScale) points
        scaledRads = Data.Vector.map (* radScale) radii
    return $ Data.Vector.toList $ Data.Vector.zipWith Sphere scaledPnts scaledRads

main :: IO ()
main = do
    gen <- MWC.create -- fixed generator

    -- create some random spheres to use as a scene
    spheres <- randomSpheres gen 50 100 20
    let scene = map (\s -> Object {shape=s, material=Paraffin}) spheres

    -- TODO: should it be Int?
    intensities <- H.new :: IO (HashTable (Int, Int, Int) Float)
    replicateM_ 1000000 $ simulate gen intensities source scene

    -- dumpHashTable intensities

    -- TODO: make writePaths work again
    -- writePaths "paths.obj" source $ map (map fst) results

    writeVolume intensities (100,100,100)
