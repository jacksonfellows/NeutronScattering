module Main where

import Control.Monad (replicateM)
import System.Random (mkStdGen, setStdGen)

import Linear
import Scatter
import Shapes
import Volume

source = V3 50 50 50
scene =
    [ Object { shape = Sphere (V3 50 50 70) 10
             , name = "top sphere"
             }
    , Object { shape = Sphere (V3 50 50 30) 10
             , name = "bottom sphere"
             }
    ]

main :: IO ()
main = do
    setStdGen $ mkStdGen 871

    results <- replicateM 1000 $ simulate source scene
    let points = concat results
        sourceVolume = addToVolume source 1 emptyVolume
        -- TODO: use strict left fold?
        vol = foldl (\vol (pos, val) -> (addToVolume pos val vol)) sourceVolume points

    writeVolume 100 100 100 vol
