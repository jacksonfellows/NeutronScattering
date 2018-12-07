module Main where

import           Control.Monad        (replicateM_)
import           Data.Attoparsec.Text
import qualified Data.HashTable.IO    as H
import           Data.Text.IO         (readFile)
import           Data.Vec3
-- import           Data.Vector.Unboxed  ((!))
import qualified Data.Vector.Unboxed  as V
import           Graphics.Formats.STL
import           System.Environment
import           System.Random.MWC    as MWC

import           Mesh
import           Scatter
import           Volume

showTriangle (Triangle norm verts) = "norm: " ++ (show norm) ++ ", verts: " ++ (show verts)

source = CVec3 0 0 0
_paraffin_ = MkMat { getSigmaScat = const 0.8, getSigmaTot = const 0.2, getName = "paraffin" }

main :: IO ()
main = do
    -- TODO: move all model reading to Mesh.hs
    bunny <- Data.Text.IO.readFile "bunny.stl"
    putStrLn "read bunny"
    let res = parseOnly stlParser bunny
        Right tris = fmap triangles res

        mesh = fromTris tris
        scene = [MkObject mesh _paraffin_]

    putStrLn $ "# of triangles: " ++ show (V.length (getTris mesh))

    gen <- MWC.create -- fixed generator

    -- TODO: should it be Int?
    intensities <- H.new :: IO (HashTable (Int, Int, Int) Float)
    putStrLn "about to simulate"

    (n:_) <- getArgs
    replicateM_ (read n) $ simulate gen intensities source scene

    -- dumpHashTable intensities
    writeVolume intensities ((-50,50),(-50,50),(-50,50))
