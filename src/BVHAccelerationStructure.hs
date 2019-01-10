{-# LANGUAGE BangPatterns #-}

-- TODO: explicit export list
module BVHAccelerationStructure where

import           Control.Applicative   ((<|>))
import           Data.List             (foldl', foldl1')
import           Data.Vec3
import qualified Data.Vector.Unboxed   as V
import           Prelude               hiding (zipWith)

import           AABB
import           AccelerationStructure
import           Ray
import           Triangle

data BVHStructure = BVHNode !AABB [BVHStructure]
                  | BVHLeaf !AABB [Triangle]
                  deriving (Show)

getBVHAABB :: BVHStructure -> AABB
getBVHAABB (BVHNode aabb _) = aabb
getBVHAABB (BVHLeaf aabb _) = aabb

getAABBs :: V.Vector Triangle -> (AABB, V.Vector AABB)
-- TODO: move both of these operations to one fold
getAABBs !tris = (V.foldl1' union aabbs, aabbs)
    where aabbs = V.map aabbFromTri tris

getCentroids :: V.Vector Triangle -> V.Vector CVec3
getCentroids = V.map getCentroid

data Octree = OctreeDummy !AABB
            | OctreeNode !AABB [Octree]
            | OctreeLeaf !AABB [Int]
            deriving (Show)

getAABB :: Octree -> AABB
getAABB (OctreeDummy aabb)  = aabb
getAABB (OctreeNode aabb _) = aabb
getAABB (OctreeLeaf aabb _) = aabb

createEmptyTree :: AABB -> Octree
createEmptyTree aabb = OctreeNode aabb $ map OctreeDummy $ octSplitAABB aabb

octSplitAABB :: AABB -> [AABB]
octSplitAABB (MkAABB (CVec3 minX minY minZ) (CVec3 maxX maxY maxZ)) =

    [ MkAABB (CVec3 minX minY minZ) (CVec3 centerX centerY centerZ)
    , MkAABB (CVec3 centerX minY minZ) (CVec3 maxX centerY centerZ)
    , MkAABB (CVec3 minX centerY minZ) (CVec3 centerX maxY centerZ)
    , MkAABB (CVec3 centerX centerY minZ) (CVec3 maxX maxY centerZ)

    , MkAABB (CVec3 minX minY centerZ) (CVec3 centerX centerY maxZ)
    , MkAABB (CVec3 centerX minY centerZ) (CVec3 maxX centerY maxZ)
    , MkAABB (CVec3 minX centerY centerZ) (CVec3 centerX maxY maxZ)
    , MkAABB (CVec3 centerX centerY centerZ) (CVec3 maxX maxY maxZ) ]

    where centerX = (minX + maxX) / 2
          centerY = (minY + maxY) / 2
          centerZ = (minZ + maxZ) / 2

maxDepth = 16 :: Int

insert :: Octree -> Int -> V.Vector CVec3 -> Octree
insert tree i centroids = insert' tree i centroids 0

insert' :: Octree -> Int -> V.Vector CVec3 -> Int -> Octree
insert' (OctreeNode !aabb !children) i centroids depth = OctreeNode aabb newChildren
    where newChildren = map (\child -> if (getAABB child) `containsPoint` (centroids V.! i)
                                       then insert' child i centroids (depth+1)
                                       else child
                                       ) children
insert' (OctreeLeaf !aabb !is) i centroids depth =
    if depth < maxDepth
    then foldl' (\tree i -> insert' tree i centroids (depth+1)) (createEmptyTree aabb) newIndices
    else OctreeLeaf aabb newIndices
    where newIndices = i : is
insert' (OctreeDummy !aabb) i _ _ = OctreeLeaf aabb [i]

maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMin a b = min <$> a <*> b <|> a <|> b

buildBVHFromOctree :: Octree -> V.Vector Triangle -> V.Vector AABB -> BVHStructure
buildBVHFromOctree (OctreeNode _ !children) !tris !aabbs = BVHNode newAABB bvhs
    where newAABB = foldl1' union $ map getBVHAABB bvhs
          bvhs = map (\o -> buildBVHFromOctree o tris aabbs) notDummies
          notDummies = filter (not . isDummy) children
          isDummy (OctreeDummy _) = True
          isDummy _               = False
buildBVHFromOctree (OctreeLeaf _ !indices) !tris !aabbs = BVHLeaf newAABB newTris
    where newAABB = foldl1' union $ map (aabbs V.!) indices
          newTris = map (tris V.!) indices

-- TODO: also check if leafs contain their triangles
nodesContainChildren :: BVHStructure -> Bool
nodesContainChildren (BVHNode aabb children) = containedByAABB aabb children &&
    all nodesContainChildren children
nodesContainChildren (BVHLeaf aabb tris) = all (containsTri aabb) tris

containedByAABB :: AABB -> [BVHStructure] -> Bool
containedByAABB aabb bvhs = all (contains aabb . getBVHAABB) bvhs

-- TODO: no more pretty Maybe monad code :(
instance AccelerationStructure BVHStructure where
    ray `intersect` (BVHNode !aabb !children) =
        if ray `intersects` aabb
        then foldl1' (\(closest,totChecked,totHit) (int,numChecked,numHit) ->
            (maybeMin closest int, totChecked + numChecked, totHit + numHit))
            $ map (intersect ray) children
        else (Nothing, 0, 0)
    ray@(MkRay !o !d) `intersect` (BVHLeaf !aabb !tris) =
        if ray `intersects` aabb
        then let (minDist,totChecked,totHit) = foldl' (\(minDist,totChecked,totHit) t ->
                     (maybeMin minDist t, totChecked + 1, if t == Nothing then totHit else totHit + 1))
                     (Nothing, 0, 0) $ map (intersectTri ray) tris
             in case minDist of
                 Just t  -> (Just $ MkIntersection (o <+> (d .^ t)) t, totChecked, totHit)
                 Nothing -> (Nothing, totChecked, totHit)
        else (Nothing, 0, 0)

    build !tris = buildBVHFromOctree octree trisVec aabbs
        where octree = foldl (\tree i -> insert tree i centroids) (createEmptyTree aabb) [0..V.length trisVec - 1]
              (aabb,aabbs) = getAABBs trisVec
              centroids = getCentroids trisVec
              trisVec = V.fromList tris
