{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: explicit export list
module BVHAccelerationStructure where

import           Control.Applicative ((<|>))
import           Data.List           (foldl', foldl1')
import qualified Data.Vector         as V
import           Linear.V3
import           Prelude             hiding (zipWith)

import           AABB
import           Intersect
import           Ray

data BVHStructure i = BVHNode !AABB [BVHStructure i]
                  | BVHLeaf !AABB [i]
                  deriving (Show)

getBVHAABB :: BVHStructure i -> AABB
getBVHAABB (BVHNode aabb _) = aabb
getBVHAABB (BVHLeaf aabb _) = aabb

getAABBs :: IntersectionPrim i => V.Vector i -> (AABB, V.Vector AABB)
-- TODO: move both of these operations to one fold
getAABBs !prims = (V.foldl1' union aabbs, aabbs)
    where aabbs = V.map buildAABBPrim prims

getCentroids :: IntersectionPrim i => V.Vector i -> V.Vector (V3 Double)
getCentroids = V.map getCentroidPrim

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
octSplitAABB box =

    [ aabb (V3 minX minY minZ) (V3 centerX centerY centerZ)
    , aabb (V3 centerX minY minZ) (V3 maxX centerY centerZ)
    , aabb (V3 minX centerY minZ) (V3 centerX maxY centerZ)
    , aabb (V3 centerX centerY minZ) (V3 maxX maxY centerZ)

    , aabb (V3 minX minY centerZ) (V3 centerX centerY maxZ)
    , aabb (V3 centerX minY centerZ) (V3 maxX centerY maxZ)
    , aabb (V3 minX centerY centerZ) (V3 centerX maxY maxZ)
    , aabb (V3 centerX centerY centerZ) (V3 maxX maxY maxZ) ]

    where centerX = (minX + maxX) / 2
          centerY = (minY + maxY) / 2
          centerZ = (minZ + maxZ) / 2
          (V3 minX minY minZ) = getMin box
          (V3 maxX maxY maxZ) = getMax box

maxDepth = 16 :: Int

insert :: Octree -> Int -> V.Vector (V3 Double) -> Octree
insert tree i centroids = insert' tree i centroids 0

insert' :: Octree -> Int -> V.Vector (V3 Double) -> Int -> Octree
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

buildBVHFromOctree :: IntersectionPrim i => Octree -> V.Vector i -> V.Vector AABB -> BVHStructure i
buildBVHFromOctree (OctreeNode _ !children) !prims !aabbs = BVHNode newAABB bvhs
    where newAABB = foldl1' union $ map getBVHAABB bvhs
          bvhs = map (\o -> buildBVHFromOctree o prims aabbs) notDummies
          notDummies = filter (not . isDummy) children
          isDummy (OctreeDummy _) = True
          isDummy _               = False
buildBVHFromOctree (OctreeLeaf _ !indices) !prims !aabbs = BVHLeaf newAABB newTris
    where newAABB = foldl1' union $ map (aabbs V.!) indices
          newTris = map (prims V.!) indices

-- for testing
-- TODO: also check if leafs contain their primitives
nodesContainChildren :: IntersectionPrim i => BVHStructure i -> Bool
nodesContainChildren (BVHNode aabb children) = containedByAABB aabb children &&
    all nodesContainChildren children
nodesContainChildren (BVHLeaf aabb prims) = all (contains aabb) $ map buildAABBPrim prims

containedByAABB :: AABB -> [BVHStructure i] -> Bool
containedByAABB aabb bvhs = all (contains aabb . getBVHAABB) bvhs

-- TODO: move, along with maybeMin, to some sort of helpers file
maybeMinOnFst a b = minFst <$> a <*> b <|> a <|> b
minFst a b = if fst a < fst b then a else b

instance IntersectionPrim i => AccelerationStructure BVHStructure i where
    ray `intersect` (BVHNode !aabb !children) =
        if ray `intersects` aabb
        then foldl1' maybeMinOnFst $ map (intersect ray) children
        else Nothing
    ray `intersect` (BVHLeaf !aabb !prims) =
        if ray `intersects` aabb
        then foldl1' maybeMinOnFst $ map (\p -> (,) <$> ray `intersectPrim` p <*> return p) prims
        else Nothing

    construct !prims = buildBVHFromOctree octree primsVec aabbs
        where octree = foldl (\tree i -> insert tree i centroids) (createEmptyTree aabb) [0..V.length primsVec - 1]
              (aabb,aabbs) = getAABBs primsVec
              centroids = getCentroids primsVec
              primsVec = V.fromList prims

    buildAABB = getBVHAABB
