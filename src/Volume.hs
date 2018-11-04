module Volume
    ( emptyVolume
    , addToVolume
    , writeVolume
    ) where

import Codec.Picture
import qualified Data.Map as Map
import Text.Printf (printf)
import Linear

-- create an empty volume map
emptyVolume :: Map.Map (V3 Int) Float
emptyVolume = Map.empty

-- add the given value at the given coordinates to the 3D volume map
addToVolume :: RealFrac a => V3 a -> Float -> Map.Map (V3 Int) Float -> Map.Map (V3 Int) Float
addToVolume pos val = Map.insertWith (+) newV val
    where
        toPixel = floor
        newV = fmap toPixel pos

-- write the volume as a series of slices along the z axis
-- TODO: set dir and prefix
writeVolume :: Int -> Int -> Int -> Map.Map (V3 Int) Float -> IO ()
writeVolume xMax yMax zMax vol = mapM_ (\(image,z) -> saveTiffImage ("scene\\slice" ++ printf "%02d" z ++ ".tiff") image) images
    where
        images = map (\z -> (makeImage xMax yMax z vol, z)) [0..zMax]


-- create an image from a specific z slice
makeImage :: Int -> Int -> Int -> Map.Map (V3 Int) Float -> DynamicImage
makeImage xMax yMax z vol = ImageYF $ generateImage (\x y -> Map.findWithDefault 0 (V3 x y z) vol) xMax yMax
