module Object
    ( Object(..)
    , Material(..)
    ) where

import           Shape

data Object = MkObject
    { getShape :: Shape
    , getMat   :: Material
    }

instance Show Object where show (MkObject _ m) = show m

-- ugly hack to allow me to sort (Intersection,Object) pairs
-- a runtime error will be thrown if it two objects are actually compared
instance Eq Object where
    _ == _ = undefined

instance Ord Object where
    _ <= _ = undefined

data Material = MkMat
    { getSigmaScat :: Double -> Double
    , getSigmaTot  :: Double -> Double
    , getName      :: String }

instance Show Material where
    show MkMat {getName=name} = name
