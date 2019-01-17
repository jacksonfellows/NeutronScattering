{-# LANGUAGE BangPatterns #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad            (replicateM_, when)
import           Control.Monad.ST
import           Data.Attoparsec.Text
import           Data.STRef
import           Data.Text.IO             (readFile)
import           Data.Vec3
import qualified Data.Vector.Storable     as V
import           GHC.Float                (float2Double)
import qualified Graphics.Formats.STL     as STL
import           System.CPUTime           (getCPUTime)
import           System.Environment       (getArgs)
import           System.IO
import           System.Random.MWC        as MWC
import           Text.Printf              (printf)

import           BVHAccelerationStructure
import           NaiveAccelerationStructure
import           Intersect
import           Object
import           Scatter
import           Triangle

showTriangle (STL.Triangle norm verts) = "norm: " ++ (show norm) ++ ", verts: " ++ (show verts)

source = CVec3 0 0 0
_paraffin_ = MkMat { getSigmaScat = const 0.8, getSigmaTot = const 0.2, getName = "paraffin" }

addToImage :: MutableImage s Pixel8 -> ((Int,Int),(Int,Int),(Int,Int)) -> (Int,Int,Int) -> CVec3 -> Pixel8 -> ST s ()
addToImage img ((minX,maxX),(minY,maxY),(minZ,maxZ)) (width,height,depth) pnt pix =
    when inImg $ writePixel img iX iY pix
    where (x,y,z) = toXYZ pnt
          iX = round x - minX
          iY = round y - minY + (round z - minZ) * depth
          inImg = 0 <= iX && iX < width && 0 <= iY && iY < height * depth

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    -- TODO: move all model reading to Mesh.hs
    bunny <- Data.Text.IO.readFile "bunny.stl"
    putStrLn "reading model..."
    let res = parseOnly STL.stlParser bunny
        Right tris = fmap STL.triangles res
        !numTris = length tris

        toVecs (STL.Triangle _ (a,b,c)) = (toVec a, toVec b, toVec c)
        toVec (x,y,z) = fromXYZ (float2Double x, float2Double y, float2Double z)
        !mesh = map (tri . toVecs) tris
        !bvh = construct mesh :: BVHStructure Triangle
        !obj = object (AnyIntersectable bvh) _paraffin_
        !scene = construct [obj] :: NaiveStructure Object

    n <- getArgs >>= return . read . head

    let (minX,maxX) = (-50,50)
        (minY,maxY) = (-50,50)
        (minZ,maxZ) = (-50,50)

        width = maxX - minX
        height = maxY - minY
        depth = maxZ - minZ

    putStrLn "scattering..."
    start <- getCPUTime

    (img, stats) <- MWC.withSystemRandom . asGenST $ \gen -> do
        img <- createMutableImage width (height * depth) 0
        let adder = addToImage img ((minX,maxX),(minY,maxY),(minZ,maxZ)) (width,height,depth)

        stats <- emptyStats
        let simState = MkSimState gen adder stats
        replicateM_ n $ simulate simState source (Just obj) (AnyIntersectableScene scene)

        frozenStats <- freezeStats stats
        frozenImg <- unsafeFreezeImage img
        return (frozenImg, frozenStats)

    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)

    putStrLn "...finished"

    putStrLn ""
    putStrLn $ printf "Scattering time: %0.3f seconds" (diff :: Double)
    putStrLn $ printf "# of triangles: %d" numTris
    putStrLn $ printf "# of neutrons: %d" n
    putStrLn $ printf "# of scattering events: %d" (getNumScattered stats)
    putStrLn $ printf "# of absorption events: %d" (getNumAbsorbed stats)
    -- putStrLn $ printf "# of ray-triangle tests: %d" (getNumTests stats)
    -- putStrLn $ printf "# of ray-triangle intersections: %d" (getNumInts stats)
    putStrLn ""

    -- startBVH <- getCPUTime
    -- let !bvh = build mesh :: BVHStructure
    -- print bvh
    -- endBVH <- getCPUTime
    -- let diffBVH = (fromIntegral (endBVH - startBVH)) / (10^12)
    -- putStrLn $ printf "Time to make BVH: %0.3f seconds" (diffBVH :: Double)

    -- cut this big image into slices that can be used by slicer
    let dat = imageData img
        step = V.length dat `div` depth
        slices = [ Image width height (V.slice i step dat) | i <- [0,step..(V.length dat - 1)] ]

    -- TODO: use generic file path separator
    mapM_ (\(img,n) -> savePngImage (printf "scene\\slice%02d.png" n) (ImageY8 img)) $ zip slices [(0::Int)..]
