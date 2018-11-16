module Main where

import           Control.Monad     (replicateM)
import qualified Data.HashTable.IO as H
import           System.Random     (mkStdGen, setStdGen)

import           Linear
import           Paths
import           Scatter
import           Shapes
import           Types
import           Volume

source = V3 50 50 50
scene =
    [ Object { shape = Sphere (V3 50 50 70) 10
             , material = Paraffin
             }
    , Object { shape = Sphere (V3 50 50 30) 10
             , material = Paraffin
             }
    ]

main :: IO ()
main = do
    setStdGen $ mkStdGen 871

    -- TODO: should it be Int?
    intensities <- H.new :: IO (HashTable (Int, Int, Int) Float)
    replicateM 100 $ simulate intensities source scene

    dumpHashTable intensities

    -- TODO: make writePaths work again
    -- writePaths "paths.obj" source $ map (map fst) results

    writeVolume intensities (100,100,100)
