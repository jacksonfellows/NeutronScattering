module Ray
    ( buildRay, Ray(..) ) where

import           Linear.V3

buildRay :: V3 Double -> V3 Double -> Ray
buildRay o dir = MkRay o dir (1 / dir)

data Ray = MkRay { getO, getDir, getInvDir :: !(V3 Double) }
    deriving (Show)

