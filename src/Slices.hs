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
import           Control.Monad               (unless, when)
import           Control.Monad.ST
import           Control.Monad.Trans.Class   (lift)
import           Data.Bifunctor              (first)
import           Data.Maybe                  (fromJust)
import qualified Data.Vector.Storable        as V
import           Data.Word
import           GHC.Float                   (double2Float)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           Text.Printf                 (printf)

import           AABB
import           Ray

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

instance Epsilon FFloat where
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

    let makePrimitives = do
            vertexArray <- newVertexArray vertexBuffer
            indexArray <- newIndexArray indexBuffer Nothing
            return $ toPrimitiveArrayIndexed TriangleList indexArray vertexArray

    uniform :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1

    shader <- compileShader $ do
        primitiveStream <- toPrimitiveStream primitives
        modelViewProj <- getUniform (const (uniform,0))
        let primitiveStream' = fmap (first (modelViewProj !*)) primitiveStream
        frontFrags :: FragmentStream (V3 FFloat) <- rasterize rasterOptions primitiveStream'
        drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) frontFrags

    loop win shader makePrimitives uniform

loop win shader makePrimitives uniform = do
    t <- lift GLFW.getTime >>= return . double2Float . fromJust
    Just (w,h) <- GLFW.getWindowSize win
    let camPos = V3 ((sin t) * 10) 5 ((cos t) * 10)
        center = V3 0 0 0
        up = V3 0 1 0
        viewMat = lookAt camPos center up
        fov = pi/3
        projMat = perspective fov (fromIntegral w / fromIntegral h) 1 100
        viewProjMat = projMat !*! viewMat
    writeBuffer uniform 0 [viewProjMat]

    render $ do
        clearWindowColor win 0
        prims <- makePrimitives
        shader $ ShaderEnvironment prims (Front, ViewPort 0 (V2 w h), DepthRange 0 1)
    swapWindowBuffers win

    closeRequested <- GLFW.windowShouldClose win
    unless (closeRequested == Just True) $
        loop win shader makePrimitives uniform

data ShaderEnvironment = ShaderEnvironment
    { primitives    :: PrimitiveArray Triangles (B4 Float, B3 Float)
    , rasterOptions :: (Side, ViewPort, DepthRange)
    }
