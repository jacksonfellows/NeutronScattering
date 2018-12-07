module Tests.Mesh
    ( tests
    -- for testing
    , meshFromAABB
    , intersectRay
    , intersect
    ) where

import           Data.Vec3
import qualified Data.Vector.Unboxed   as V
import           Test.Tasty
-- import Test.Tasty.HUnit
import           Prelude               hiding (zipWith)
import Data.Maybe (fromJust)
import           Test.Tasty.QuickCheck as QC

import           AABB
import           Mesh
import           Ray
import           Shape
import           Tests.AABB            hiding (tests)

tests = testGroup "Mesh" [intersectsTests]

intersectsTests = testGroup "intersects" [propertyTests]

propertyTests = testGroup "Property Tests"
    [ QC.testProperty "The intersectRay function I created for AABBs returns something when intersect returns true and nothing otherwise" $
        \aabb (MkUnitVector dir) (MkPosVec3 o) -> let ray = MkRay o dir in intsMatch ray aabb

    , QC.testProperty "The intersection of a ray and a AABB is the same as a ray and the 12 triangles that make up that AABB" $
        \aabb (MkUnitVector dir) -> let ray = MkRay (CVec3 0 0 0) dir in (ray `intersectRay` aabb) `aboutEqualsInt` (ray `intersect` (meshFromAABB aabb))
    ]

intsMatch ray aabb
    | int == Nothing = not doesInt
    | otherwise = doesInt
    where doesInt = ray `intersects` aabb
          int = ray `intersectRay` aabb

aboutEqualsInt a b
    | a == Nothing && b == Nothing = True -- both don't intersect
    | a == Nothing || b == Nothing = False -- one intersects and one doesn't
    | otherwise = let i0 = fromJust a
                      i1 = fromJust b
                  in abs (getDist i0 - getDist i1) < 1e-5 && abs ((getPoint i0) `distance` (getPoint i1)) < 1e-5

-- TODO: ugly not only to duplicate work but to not make an instance of Shape
-- But this function is currently useful for testing Mesh
intersectRay :: Ray -> AABB -> Maybe Intersection
(MkRay o d) `intersectRay` (MkAABB b0 b1)
    | tmin > tmax = Nothing
    | tmin > 0 = Just $ MkIntersection (o <+> (d .^ tmin)) tmin
    | tmax > 0 = Just $ MkIntersection (o <+> (d .^ tmax)) tmax
    | otherwise = Nothing
    where (tmin, tmax) = foldl f ((-1)/0, 1/0) $ zip (ints b0) (ints b1)
          f (tmin, tmax) (t0, t1) = (max tmin (min t0 t1), min tmax (max t0 t1))
          ints b = [x,y,z]
              where (x,y,z) = toXYZ i
                    i = zipWith (/) (b <-> o) d
--   g - h
--  /|  /|       y  z
-- c - d |       | /
-- | e-|-f       |/
-- a - b/        +---x

-- TODO: there has to be a better way of doing this than enumerating each possibility
meshFromAABB :: AABB -> Mesh
meshFromAABB (MkAABB a h) = MkMesh $ V.fromList sides
    where sides = [(a,b,d), (a,c,d), (a,b,f), (a,e,f), (a,e,g), (a,c,g), (h,g,c), (h,d,c), (h,f,b), (h,d,b), (h,g,e), (h,f,e)]
          (x0,y0,z0) = toXYZ a
          (x1,y1,z1) = toXYZ h
          b = CVec3 x1 y0 z0
          c = CVec3 x0 y1 z0
          d = CVec3 x1 y1 z0
          e = CVec3 x0 y0 z1
          f = CVec3 x1 y0 z1
          g = CVec3 x0 y1 z1
