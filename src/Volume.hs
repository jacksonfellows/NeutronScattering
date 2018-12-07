module Volume
    ( HashTable
    , dumpHashTable
    , writeVolume
    , addToVolume
    ) where

import           Codec.Picture
import           Data.Hashable
import qualified Data.HashTable.IO as H
import           Data.Vec3
import           System.IO.Unsafe  (unsafePerformIO)
import           Text.Printf       (printf)


type HashTable k v = H.CuckooHashTable k v

-- helper function that prints all key value pairs in a hash table
dumpHashTable :: (Eq k, Hashable k, Show k, Show v) => HashTable k v -> IO ()
dumpHashTable = H.mapM_ printPair
    where printPair = \(k, v) -> do putStrLn $ (show k) ++ ": " ++ (show v)

-- write the volume as a series of slices along the z axis
-- TODO: set dir and prefix
writeVolume :: HashTable (Int, Int, Int) Float -> ((Int,Int), (Int,Int), (Int,Int)) -> IO ()
writeVolume intensities ((xMin,xMax), (yMin,yMax), (zMin,zMax)) = mapM_ saveImage images
    where saveImage (image,z) = saveTiffImage ("scene\\slice" ++ printf "%02d" (z-zMin) ++ ".tiff") image
          images = map (\z -> (makeImage intensities ((xMin,xMax), (yMin,yMax), z), z)) [zMin..zMax]

-- create an image from a specific z slice
-- TODO: looks god-awful
makeImage :: HashTable (Int, Int, Int) Float -> ((Int,Int), (Int,Int), Int) -> DynamicImage
makeImage intensities ((xMin,xMax), (yMin,yMax), z) = ImageYF $ generateImage getPixel (xMax-xMin) (yMax-yMin)
    where getPixel x y = let val = unsafePerformIO $ H.lookup intensities $ (x+xMin, y+yMin, z)
                         in case val of
                            Nothing  -> 0
                            (Just v) -> v

-- convert a point to a key
toKey :: CVec3 -> (Int, Int, Int)
toKey v = let (x, y, z) = toXYZ v in (floor x, floor y, floor z)

-- add a point and intensity to the volume
addToVolume :: HashTable (Int, Int, Int) Float -> CVec3 -> Float -> IO ()
addToVolume intensities point val = do
    let key = toKey point
    prev <- H.lookup intensities key
    case prev of
        Nothing -> H.insert intensities key val
        Just v  -> H.insert intensities key (val + v)
