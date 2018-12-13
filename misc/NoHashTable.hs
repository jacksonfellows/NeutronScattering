module NoHashTable where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad       (replicateM_, when)
import           Control.Monad.ST
import           System.Random.MWC   as MWC

main :: IO ()
main = do
    img <- MWC.withSystemRandom . asGenST $ \gen -> makeCircleImg gen 100000 (800, 800)
    savePngImage "circle_test.png" $ ImageY8 img

makeCircleImg :: GenST s -> Int -> (Int,Int) -> ST s (Image Pixel8)
makeCircleImg gen n (width,height) = do
    img <- createMutableImage width height 0
    replicateM_ n $ do
        x <- uniformR (0,width) gen
        y <- uniformR (0,height) gen
        when (inCircle x y) $ writePixel img x y 255
    unsafeFreezeImage img
    where inCircle x y = let cX = width `div` 2
                             cY = height `div` 2
                             r = (min width height) `div` 4
                         in (x - cX)^2 + (y - cY)^2 < r^2
