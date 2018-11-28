module Ray
    ( Ray(..) ) where

import Data.Vec3

data Ray = MkRay
    { getO, getDir :: CVec3 }
    deriving (Show)

