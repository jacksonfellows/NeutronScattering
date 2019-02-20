{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Slices
    ( Slices
    , Adder
    , initFromAABB
    , addToSlices
    , freezeAndSlice
    , writeSlices
    , display
    ) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Arrow               (first)
import           Control.Monad               (unless, when)
import           Control.Monad.ST
import           Control.Monad.Trans.Class   (lift)
import           Data.Maybe                  (fromJust)
import qualified Data.Vector.Storable        as V
import           Data.Word
import           GHC.Float                   (double2Float)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           Text.Printf                 (printf)

import           AABB

type Val = Pixel8
type Adder s = V3 Double -> Val -> ST s ()
data Slices s = MkSlices AABB (MutableImage s Val)

toXYZ (V3 x y z) = (x,y,z)
floored (x,y,z) = (floor x,floor y,floor z)
getDims box = floored $ toXYZ $ getMax box - getMin box

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
freezeAndSlice :: Slices s -> ST s [Codec.Picture.Types.Image Val]
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

-- TODO: HACK
instance Epsilon VFloat where
    nearZero _ = False

display :: IO ()
display = runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Hello World!")

    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 8
    writeBuffer vertexBuffer 0 [ (V4 (-1) (-1) 1 1, V3 (-1) (-1) 1)
                               , (V4 1 (-1) 1 1, V3 1 (-1) 1)
                               , (V4 1 1 1 1, V3 1 1 1)
                               , (V4 (-1) 1 1 1, V3 (-1) 1 1)
                               , (V4 (-1) (-1) (-1) 1, V3 (-1) (-1) (-1))
                               , (V4 1 (-1) (-1) 1, V3 1 (-1) (-1))
                               , (V4 1 1 (-1) 1, V3 1 1 (-1))
                               , (V4 (-1) 1 (-1) 1, V3 (-1) 1 (-1)) ]

    indexBuffer :: Buffer os (BPacked Word8) <- newBuffer (12*3)
    writeBuffer indexBuffer 0 [ -- front
                                0, 1, 2
                              , 2, 3, 0
                                -- right
                              , 1, 5, 6
                              , 6, 2, 1
                                -- back
                              , 7, 6, 5
                              , 5, 4, 7
                                -- left
                              , 4, 0, 3
                              , 3, 7, 4
                                -- bottom
                              , 4, 5, 1
                              , 1, 0, 4
                                -- top
                              , 3, 2, 6
                              , 6, 7, 3 ]

    uniformBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1

    shader <- compileShader $ do
        primitiveStream <- toPrimitiveStream id
        camPos <- getUniform (const (uniformBuffer,0))
        let viewMat = lookAt camPos (V3 0 0 0) (V3 0 1 0)
            projMat = perspective (pi/3) 1 1 100
            primitiveStream' = fmap (first (projMat !*! viewMat !*)) primitiveStream
        fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream'
        drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer indexBuffer shader win uniformBuffer

loop vertexBuffer indexBuffer shader win uniformBuffer = do
    t <- lift GLFW.getTime >>= return . double2Float . fromJust
    writeBuffer uniformBuffer 0 [V3 ((sin t) * 10) 5 ((cos t) * 10)]
    render $ do
        clearWindowColor win (V3 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        indexArray <- newIndexArray indexBuffer Nothing
        let primitiveArray = toPrimitiveArrayIndexed TriangleList indexArray vertexArray
        shader primitiveArray
    swapWindowBuffers win

    closeRequested <- GLFW.windowShouldClose win
    unless (closeRequested == Just True) $
        loop vertexBuffer indexBuffer shader win uniformBuffer
