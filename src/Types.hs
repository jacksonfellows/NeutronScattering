{-# LANGUAGE NamedFieldPuns #-}

module Types
    ( Neutron(..)
    , Ray(..)
    , Object(..)
    , Material(..)
    , Shape(..)
    , QuadraticSolution(..)
    , Intersection(..)
    ) where

import           Data.Vec3 hiding (origin)

-- TODO: parametrize these types?

-- is the energy the magnitude of the direction?
data Neutron = Neutron
    { ray    :: Ray
    , inside :: Maybe Object
    }
    deriving (Show)

data Ray = Ray
    { origin, dir :: CVec3 }
    deriving (Show)

data Object = Object
    { shape    :: Shape
    , material :: Material
    }
    deriving (Show, Eq)

-- TODO: it doesn't really make sense to "order" objects
instance Ord Object where
    _ `compare` _ = EQ

data Material = Air | Paraffin
    deriving (Show, Eq, Ord)

-- TODO: shapes are meshes with bounding boxes?
data Shape = Sphere
    { center :: CVec3
    , radius :: Double
    }
    deriving (Show, Eq)

data QuadraticSolution = Roots Double Double | Root Double | None
    deriving (Show, Eq)

data Intersection = Intersection
    { point        :: CVec3
    , distanceFrom :: Double
    }
    deriving (Show, Eq)

-- Intersections can be ordered based on closeness
instance Ord Intersection where
    (Intersection _ a) <= (Intersection _ b) = a <= b
