module Main where

import           Control.Monad     (replicateM_)
import qualified Data.HashTable.IO as H
import           Data.Vec3
import           System.Random.MWC as MWC

import           Scatter
import           Shapes
import           Types
import           Volume

source = CVec3 50 50 50
scene =
    [ Object { shape = Sphere (CVec3 50 50 70) 10
             , material = Paraffin
             }
    , Object { shape = Sphere (CVec3 50 50 30) 10
             , material = Paraffin
             }
    ]

main :: IO ()
main = do
    gen <- MWC.create -- fixed generator

    -- TODO: should it be Int?
    intensities <- H.new :: IO (HashTable (Int, Int, Int) Float)
    replicateM_ 1000000 $ simulate gen intensities source scene

    -- dumpHashTable intensities

    -- TODO: make writePaths work again
    -- writePaths "paths.obj" source $ map (map fst) results

    writeVolume intensities (100,100,100)
