module Main where

import           Control.Monad     (replicateM_)
import qualified Data.HashTable.IO as H
import           Data.Vec3
import qualified Data.Vector
import           System.Random.MWC as MWC

import           AABB
import           BVH
import           Scatter
import           Sphere
import           Volume

source = CVec3 50 50 50

-- helper function to generate random scenes for testing
randomSpheres :: GenIO -> Int -> Double -> Double-> IO [Sphere]
randomSpheres gen n pntScale radScale = do
    xs <- MWC.uniformVector gen n
    ys <- MWC.uniformVector gen n
    zs <- MWC.uniformVector gen n
    let points = Data.Vector.map fromXYZ $ Data.Vector.zip3 xs ys zs

    radii <- MWC.uniformVector gen n

    let scaledPnts = Data.Vector.map (.^ pntScale) points
        scaledRads = Data.Vector.map (* radScale) radii
    return $ Data.Vector.toList $ Data.Vector.zipWith MkSphere scaledPnts scaledRads

_paraffin_ = MkMat { getSigmaScat = const 0.8, getSigmaTot = const 0.2, getName = "paraffin" }

showCoords vec = "[" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "]"
    where (x,y,z) = toXYZ vec

translate offset item = "translate (" ++ (showCoords offset) ++ "){" ++ item ++ "}\n"

-- TODO: use printf or something
asOpenScad :: BVHTree Sphere -> String
asOpenScad (Branch aabb l r) = translate (getMin aabb) ("cube(" ++ (showCoords (getMax aabb <-> getMin aabb)) ++ ");") ++ asOpenScad l ++ asOpenScad r
asOpenScad (Leaf _ (MkObject sphere _)) = translate (getCenter sphere) ("sphere(" ++ (show (getRad sphere)) ++ ");")

main :: IO ()
main = do
    gen <- MWC.create -- fixed generator

    -- create some random spheres to use as a scene
    spheres <- randomSpheres gen 50 100 20
    let (o:os) = map (\s -> MkObject {getShape=s, getMat=_paraffin_}) spheres
        scene = foldl addToBVH (buildLeaf o) os

    -- putStrLn $ asOpenScad scene
    print $ parentsContainChildren scene

    -- TODO: should it be Int?
    -- intensities <- H.new :: IO (HashTable (Int, Int, Int) Float)
    -- replicateM_ 1000000 $ simulate gen intensities source scene

    -- dumpHashTable intensities

    -- TODO: make writePaths work again
    -- writePaths "paths.obj" source $ map (map fst) results

    -- writeVolume intensities (100,100,100)
