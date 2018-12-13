module Object
    ( Object(..)
    , Material(..)
    ) where

data Object a = MkObject
    { getShape :: !a
    , getMat   :: Material
    } deriving (Show)

-- ugly hack to allow me to sort (Intersection,Object) pairs
-- a runtime error will be thrown if it two objects are actually compared
instance Eq (Object a) where
    _ == _ = undefined

instance Ord (Object a) where
    _ <= _ = undefined

data Material = MkMat
    { getSigmaScat :: Double -> Double
    , getSigmaTot  :: Double -> Double
    , getName      :: String }

instance Show Material where
    show MkMat {getName=name} = name
