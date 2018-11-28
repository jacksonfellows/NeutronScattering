module Object
    ( Object(..)
    , Material(..)
    ) where

data Object a = MkObject
    { getShape :: a
    , getMat   :: Material
    } deriving (Show)

data Material = MkMat
    { getSigmaScat :: Double -> Double
    , getSigmaTot  :: Double -> Double
    , getName      :: String }

instance Show Material where
    show MkMat {getName=name} = name
