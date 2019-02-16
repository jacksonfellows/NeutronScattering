{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad      (replicateM_)
import           Data.Vec3
import           System.CPUTime     (getCPUTime)
import           System.Environment (getArgs)
import           System.IO
import           System.Random.MWC  as MWC
import           Text.Printf        (printf)

import           AABB
import           Intersect
import           Scatter
import           Scene
import           Slices

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    args <- getArgs
    let sceneFile = args !! 0
        numNeutrons = read $ args !! 1

    putStrLn "loading scene..."
    !scene <- parseScene sceneFile
    let sceneAABB = buildAABBScene scene `union` (aabb source source)
        source = CVec3 0 0 0 -- midpoint sceneAABB

        -- (minX,maxX) = (-100,100)
        -- (minY,maxY) = (-100,100)
        -- (minZ,maxZ) = (-100,100)

        floored (x,y,z) = (floor x,floor y,floor z)
        (minX,minY,minZ) = floored $ toXYZ $ getMin sceneAABB
        (maxX,maxY,maxZ) = floored $ toXYZ $ getMax sceneAABB

        width = maxX - minX
        height = maxY - minY
        depth = maxZ - minZ

    putStrLn $ "sceneAABB: " ++ show sceneAABB
    putStrLn $ "source: " ++ show source
    putStrLn ""
    putStrLn $ "minX: " ++ show minX
    putStrLn $ "minY: " ++ show minY
    putStrLn $ "minZ: " ++ show minZ
    putStrLn ""
    putStrLn $ "maxX: " ++ show maxX
    putStrLn $ "maxY: " ++ show maxY
    putStrLn $ "maxZ: " ++ show maxZ
    putStrLn ""
    putStrLn $ "width: " ++ show width
    putStrLn $ "height: " ++ show height
    putStrLn $ "depth: " ++ show depth
    putStrLn ""

    putStrLn "scattering..."
    start <- getCPUTime

    (slices, stats) <- MWC.withSystemRandom . asGenST $ \gen -> do
        slices <- initFromAABB sceneAABB
        let adder = addToSlices slices

        stats <- emptyStats
        let simState = MkSimState gen adder stats
        replicateM_ numNeutrons $ simulate simState source Nothing scene

        frozenStats <- freezeStats stats
        frozenSlices <- freezeAndSlice slices
        return (frozenSlices, frozenStats)

    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)

    putStrLn "...finished"

    putStrLn ""
    putStrLn $ printf "Scattering time: %0.3f seconds" (diff :: Double)
    -- putStrLn $ printf "# of triangles: %d" numTris
    putStrLn $ printf "# of neutrons: %d" numNeutrons
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

    putStrLn $ "writing slices..."
    writeSlices slices
