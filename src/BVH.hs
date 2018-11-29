module BVH
    ( BVHTree(..)
    , intersectBVH
    , addToBVH
    , buildLeaf
    , getAABB
    , parentsContainChildren
    ) where

import           Control.Applicative ((<|>))
import           Data.Vec3

import           AABB
import           Object
import           Shape

data BVHTree a = Branch AABB (BVHTree a) (BVHTree a)
               | Leaf AABB (Object a)
               deriving (Show)

maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMin a b = min <$> a <*> b <|> a <|> b

intersectBVH :: Shape a => Ray -> BVHTree a -> Maybe (Intersection, Object a)
ray `intersectBVH` (Branch aabb l r)
    | ray `intersects` aabb = maybeMin (ray `intersectBVH` l) (ray `intersectBVH` r)
    | otherwise = Nothing
ray `intersectBVH` (Leaf aabb obj)
    | ray `intersects` aabb = do
        int <- ray `intersect` (getShape obj)
        return (int,obj)
    | otherwise = Nothing

getAABB :: BVHTree a -> AABB
getAABB (Branch aabb _ _) = aabb
getAABB (Leaf aabb _)     = aabb

addToBVH :: (Shape a) => BVHTree a -> Object a -> BVHTree a
addToBVH root obj = updateBVH newNode path
    where newLeaf = buildLeaf obj
          (bestLeaf,path) = findBestNode (lowestSurfaceArea newLeaf) root
          newNode = Branch newAABB newLeaf bestLeaf
          newAABB = union (getAABB newLeaf) (getAABB bestLeaf)

findBestNode :: NodePicker a -> BVHTree a -> (BVHTree a, [(BVHTree a,Side)])
findBestNode _ leaf@(Leaf _ _) = (leaf,[])
findBestNode picker branch@(Branch _ l r) = (best,path ++ [(branch,side)])
    where side = picker branch
          child = if side == L then l else r
          (best,path) = findBestNode picker child

buildLeaf :: (Shape a) => Object a -> BVHTree a
buildLeaf obj = Leaf aabb obj
    where aabb = buildAABB $ getShape obj

data Side = L | R
    deriving (Eq)

updateBVH :: BVHTree a -> [(BVHTree a,Side)] -> BVHTree a
updateBVH root [] = root
updateBVH newChild (((Branch aabb l r),side):xs) = updateBVH newBranch xs
    where newBranch = if side == L then Branch (union (getAABB newChild) (getAABB r)) newChild r else Branch (union (getAABB l) (getAABB newChild)) l newChild

type NodePicker a = BVHTree a -> Side

lowestSurfaceArea :: BVHTree a -> NodePicker a
lowestSurfaceArea node (Branch _ l r)
    | newA l < newA r = L
    | otherwise = R
    where newA n = surfaceArea $ union (getAABB node) (getAABB n)
          surfaceArea (MkAABB min max) = max `distance` min -- directly proportional to surface area

parentsContainChildren :: BVHTree a -> Bool
parentsContainChildren (Leaf _ _) = True
parentsContainChildren (Branch aabb l r) = all pred [l,r]
    where pred = \c -> (aabb `contains` (getAABB c)) && (parentsContainChildren c)
