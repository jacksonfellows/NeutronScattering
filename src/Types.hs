module Types
    ( Neutron(..)
    , Object(..)
    , Material(..)
    , Shape(..)
    , QuadraticSolution(..)
    , Intersection(..)
    ) where

import           Linear

-- TODO: parametrize these types?

-- is the energy the magnitude of the direction?
data Neutron = Neutron
    { ray    :: Ray Double
    , inside :: Maybe Object
    }
    deriving (Show, Read)

-- TODO: it doesn't really make sense to "order" objects
data Object = Object
    { shape    :: Shape
    , material :: Material
    }
    deriving (Show, Read, Eq, Ord)

data Material = Air | Paraffin
    deriving (Show, Read, Eq, Ord)

-- TODO: shapes are meshes with bounding boxes?
data Shape = Sphere
    { center :: V3 Double
    , radius :: Double
    }
    deriving (Show, Read, Eq, Ord)

data QuadraticSolution = Roots Double Double | Root Double | None
    deriving (Show, Read, Eq)

data Intersection = Intersection
    { point        :: V3 Double
    , distanceFrom :: Double
    }
    deriving (Show, Eq, Read)

-- Intersections can be ordered based on closeness
instance Ord Intersection where
    (Intersection _ a) <= (Intersection _ b) = a <= b
