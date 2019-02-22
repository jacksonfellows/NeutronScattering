{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

 -- TODO: don't export constructor
 module Object
    ( Object(..)
    , object
    , Material(..)
    ) where

import           Intersect

data Object n = MkObject
    { getShape :: Intersectable n
    , getMat   :: Material
    }

object :: Intersectable n -> Material -> Object n
object = MkObject

instance IntersectionPrim Object n where
    intersectPrim ray obj = intersectAny ray $ getShape obj
    buildAABBPrim obj = buildAABBAny $ getShape obj
    getCentroidPrim obj = getCentroidAny $ getShape obj

instance Show (Object n) where show (MkObject _ m) = show m

-- ugly hack to allow me to sort (Intersection,Object) pairs
-- a runtime error will be thrown if it two objects are actually compared
-- instance Eq Object where
--     _ == _ = undefined
--
-- instance Ord Object where
--     _ <= _ = undefined

data Material = MkMat
    { getSigmaScat :: Double -> Double
    , getSigmaTot  :: Double -> Double
    , getName      :: String }

instance Show Material where
    show MkMat {getName=name} = name
