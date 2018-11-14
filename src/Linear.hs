{-# LANGUAGE NamedFieldPuns #-}

module Linear
    ( V3(..)
    , dot, (.*)
    , magnitude, distance
    , randomDir
    , Ray(..)
    , pointOnRay
    ) where

import           Control.Applicative   (liftA2)
import           System.Random         (randomIO)
import           Test.Tasty.QuickCheck as QC

data V3 a = V3 a a a
    deriving (Eq, Ord, Show, Read)

instance Functor V3 where
    fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Foldable V3 where
    -- TODO: ugly
    foldr f i (V3 x y z) = f x $ f y $ f z i

instance Applicative V3 where
    pure a = V3 a a a
    (V3 a b c) <*> (V3 x y z) = V3 (a x) (b y) (c z)

instance Num a => Num (V3 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)

    abs = fmap abs
    signum = fmap signum
    negate = fmap negate

    fromInteger i = pure (fromInteger i)

-- TODO: ugly
instance QC.Arbitrary a => QC.Arbitrary (V3 a) where
    arbitrary = do
        (x,y,z) <- arbitrary
        return $ V3 x y z

-- dot product
dot :: Num a => V3 a -> V3 a -> a
(V3 a b c) `dot` (V3 x y z) = a * x + b * y + c * z

-- scalar multiplication
(.*) :: Num a => V3 a -> a -> V3 a
v .* n = fmap (*n) v

-- magnitude and distance
magnitude :: Floating a => V3 a -> a
magnitude = sqrt . sum . (fmap (**2))
distance :: Floating a => V3 a -> V3 a -> a
distance a b = magnitude (a - b)

-- random unit vector
-- TODO: does it have to be a double?
randomDir :: IO (V3 Double)
randomDir = do
    r0 <- randomIO :: IO Double
    r1 <- randomIO :: IO Double
    let theta = r0 * 2 * pi
        phi = acos $ r1 * 2 - 1
    return $ V3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

data Ray a = Ray
    { origin, dir :: V3 a }
    deriving (Show, Read)

pointOnRay :: Num a => Ray a -> a -> V3 a
pointOnRay Ray {origin, dir} t = origin + dir .* t

