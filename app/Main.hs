{-# LANGUAGE BangPatterns #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad              (replicateM_, when)
import           Control.Monad.ST
import           Data.Attoparsec.Text
import           Data.STRef
import           Data.Text.IO               (readFile)
import           Data.Vec3
import qualified Data.Vector.Storable       as V
import qualified Data.Vector.Unboxed        as U
import           GHC.Float                  (float2Double)
import           Graphics.Formats.STL
import           System.CPUTime
import           System.Environment
import           System.Random.MWC          as MWC
import           Text.Printf                (printf)

import           AccelerationStructure
import           NaiveAccelerationStructure
import           Scatter

showTriangle (Triangle norm verts) = "norm: " ++ (show norm) ++ ", verts: " ++ (show verts)

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
    -- TODO: move all model reading to Mesh.hs
    bunny <- Data.Text.IO.readFile "bunny.stl"
    putStrLn "reading model..."
    let res = parseOnly stlParser bunny
        Right tris = fmap triangles res

        toVecs (Triangle _ (a,b,c)) = (toVec a, toVec b, toVec c)
        toVec (x,y,z) = fromXYZ (float2Double x, float2Double y, float2Double z)
        !mesh = build $ map toVecs tris :: NaiveStructure

        scene = [MkObject (AnyIntersectable mesh) _paraffin_]

    gen <- MWC.create -- fixed generator

    n <- getArgs >>= return . read . head

    let (minX,maxX) = (-50,50)
        (minY,maxY) = (-50,50)
        (minZ,maxZ) = (-50,50)

        width = maxX - minX
        height = maxY - minY
        depth = maxZ - minZ

    putStrLn "scattering..."
    start <- getCPUTime

    (img, numCols) <- MWC.withSystemRandom . asGenST $ \gen -> do
        img <- createMutableImage width (height * depth) 0
        let adder = addToImage img ((minX,maxX),(minY,maxY),(minZ,maxZ)) (width,height,depth)

        numColsRef <- newSTRef 0
        let simState = MkSimState gen adder numColsRef
        replicateM_ n $ simulate simState source scene

        numCols <- readSTRef numColsRef
        frozen <- unsafeFreezeImage img
        return (frozen, numCols)

    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)

    putStrLn "...finished"

    putStrLn ""
    putStrLn $ printf "Scattering time: %0.3f seconds" (diff :: Double)
    -- putStrLn $ printf "Total # of triangles: %d" numTris
    putStrLn $ printf "# of neutrons: %d" n
    putStrLn $ printf "# of collisions (scattering and absorption): %d" numCols
    putStrLn ""

    -- cut this big image into slices that can be used by slicer
    let dat = imageData img
        step = V.length dat `div` depth
        slices = [ Image width height (V.slice i step dat) | i <- [0,step..(V.length dat - 1)] ]

    -- TODO: use generic file path separator
    mapM_ (\(img,n) -> savePngImage (printf "scene\\slice%02d.png" n) (ImageY8 img)) $ zip slices [(0::Int)..]
