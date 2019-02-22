{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scene ( parseScene ) where

import           Data.Aeson                 hiding (Object, object)
import           Data.Attoparsec.Text
import           Data.Maybe                 (fromJust)
import           Data.Text                  hiding (map)
import           Data.Text.IO               (readFile)
import           GHC.Float                  (float2Double)
import           GHC.Generics
import qualified Graphics.Formats.STL       as STL
import           Linear.V3
import           Prelude                    hiding (readFile)

import           BVHAccelerationStructure
import           Intersect
import           NaiveAccelerationStructure
import           Object
import           Triangle

-- a 3d mesh file and some attributes, listed in the scene json file
data Mesh = MkMesh {
      file   :: FilePath
    , offset :: [Double]
    } deriving (Generic, Show)

instance FromJSON Mesh

-- TODO: global materials list? Specify material in scene json?
_paraffin_ :: Material
_paraffin_ = MkMat { getSigmaScat = const 0, getSigmaTot = const 1, getName = "paraffin" }

-- parse scene description files, read and shift the relevant meshes
-- and return an IntersectableScene Object
parseScene :: FilePath -> IO (IntersectableScene Double Object)
parseScene sceneFile = do
    meshes :: [Mesh] <- decodeFileStrict sceneFile >>= return . fromJust
    objs <- mapM meshToObj meshes
    return $ AnyIntersectableScene (construct objs :: NaiveStructure Object Double)

toTri :: STL.Triangle -> Triangle Double
toTri (STL.Triangle _ (a,b,c)) = tri (toVec a, toVec b, toVec c)
    where toVec (x,y,z) = V3 (float2Double x) (float2Double y) (float2Double z)

getTris :: Text -> [Triangle Double]
getTris stlFile = tris
    where res = parseOnly STL.stlParser stlFile
          Right stlTris = fmap STL.triangles res
          tris = map toTri stlTris

toV3 :: [Double] -> V3 Double
toV3 [x,y,z] = V3 x y z
toV3 invalid = error $ "invalid vec3: " ++ show invalid

meshToObj :: Mesh -> IO (Object Double)
meshToObj MkMesh {..} = do
    stlFile <- readFile file
    let !tris = getTris stlFile
        !offsetVec = toV3 offset
        !newTris = map (<++> offsetVec) tris
        !bvh = construct newTris :: BVHStructure Triangle Double
        !obj = object (AnyIntersectable bvh) _paraffin_
    return obj
