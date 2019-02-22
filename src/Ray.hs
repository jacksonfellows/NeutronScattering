module Ray
    ( buildRay, Ray(..) ) where

import           Linear.V3

buildRay o dir = MkRay o dir (1 / dir)

data Ray a = MkRay { getO, getDir, getInvDir :: !(V3 a) }
    deriving (Show)

