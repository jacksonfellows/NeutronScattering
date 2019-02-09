module Slices
    ( Slices
    , Adder
    , initFromAABB
    , addToSlices
    , freezeAndSlice
    , writeSlices
    ) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad        (when)
import           Control.Monad.ST
import qualified Data.Vector.Storable as V
import           Text.Printf          (printf)

import           AABB
import           Data.Vec3

type Val = Pixel8
type Adder s = CVec3 -> Val -> ST s ()
data Slices s = MkSlices AABB (MutableImage s Val)

floored (x,y,z) = (floor x,floor y,floor z)
getDims box = floored $ toXYZ $ getMax box <-> getMin box

initFromAABB :: AABB -> ST s (Slices s)
initFromAABB box = do
    img <- createMutableImage width (height * depth) 0
    return $ MkSlices box img
    where (width,height,depth) = getDims box

addToSlices :: Slices s -> Adder s
addToSlices (MkSlices box img) pos val = when (box `containsPoint` pos) $
    writePixel img (x - minX) (y - minY + (z - minZ) * depth) val
    where (x,y,z) = floored $ toXYZ pos
          (width,height,depth) = getDims box
          (minX,minY,minZ) = floored $ toXYZ $ getMin box

-- cut this big image into slices that can be used by slicer
freezeAndSlice :: Slices s -> ST s [Image Val]
freezeAndSlice (MkSlices box img) = do
    frozen <- unsafeFreezeImage img
    let dat = imageData frozen
        step = V.length dat `div` depth
        (width,height,depth) = getDims box
    return $ [ Image width height (V.slice i step dat) | i <- [0,step..(V.length dat - 1)] ]

-- TODO: use generic file path separators
writeSlices slices = mapM_
    (\(img,numNeutrons) -> savePngImage (printf "slices/slice%02d.png" numNeutrons) (ImageY8 img))
    $ zip slices [(0::Int)..]
