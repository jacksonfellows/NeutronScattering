module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad        (replicateM_)
import           Data.Vec3
import qualified Data.Vector
import qualified Data.Vector.Storable as V
import           System.Random.MWC    as MWC
import           Text.Printf          (printf)

import           AABB
import           BVH
import           Scatter
import           Sphere

source = CVec3 50 50 50

-- helper function to generate random scenes for testing
randomSpheres :: GenIO -> Int -> Double -> Double -> IO [Sphere]
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

    let width = 100
        height = 100
        depth = 100

    img <- MWC.withSystemRandom . asGenST $ \gen -> do
        img <- createMutableImage width (height * depth) 0
        replicateM_ 1000000 $ simulate gen img source depth scene
        unsafeFreezeImage img

    -- cut this big image into slices that can been used by slicer
    let dat = imageData img
        step = V.length dat `div` depth
        slices = [ Image width height (V.slice i step dat) | i <- [0,step..(V.length dat - 1)] ]

    -- TODO: use generic file path separator
    mapM_ (\(img,n) -> savePngImage (printf "scene\\slice%02d.png" n) (ImageY8 img)) $ zip slices [(0::Int)..]

    -- TODO: make writePaths work again
    -- writePaths "paths.obj" source $ map (map fst) results
