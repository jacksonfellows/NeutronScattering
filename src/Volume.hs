module Volume
    ( HashTable
    , writeVolume
    ) where

import           Codec.Picture
import qualified Data.HashTable.IO as H
import           System.IO.Unsafe  (unsafePerformIO)
import           Text.Printf       (printf)

import           Linear

type HashTable k v = H.CuckooHashTable k v

-- write the volume as a series of slices along the z axis
-- TODO: set dir and prefix
writeVolume :: HashTable (Int, Int, Int) Float -> (Int, Int, Int) -> IO ()
writeVolume intensities (xMax, yMax, zMax) = mapM_ (\(image,z) -> saveTiffImage ("scene\\slice" ++ printf "%02d" z ++ ".tiff") image) images
    where
        images = map (\z -> (makeImage intensities (xMax, yMax, z), z)) [0..zMax]

-- create an image from a specific z slice
-- TODO: looks god-awful
makeImage :: HashTable (Int, Int, Int) Float -> (Int, Int, Int) -> DynamicImage
makeImage intensities (xMax, yMax, z) = ImageYF $ generateImage getPixel xMax yMax
    where getPixel x y = let val = unsafePerformIO $ H.lookup intensities $ (x, y, z) in if val == Nothing then 0 else let (Just v) = val in v
