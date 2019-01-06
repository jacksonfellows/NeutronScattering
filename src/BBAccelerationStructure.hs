{-# LANGUAGE BangPatterns #-}

module BBAccelerationStructure
    ( BBStructure
    -- for debugging
    , getAABB
    ) where

import           Control.Applicative   ((<|>))
import           Data.Vec3
import qualified Data.Vector.Unboxed   as V
import           Prelude               hiding (zipWith)

import           AABB
import           AccelerationStructure
import           Ray
import           Triangle

newtype BBStructure = MkN (V.Vector Triangle)

getAABB :: BBStructure -> AABB
getAABB (MkN !tris) = MkAABB low high
    where (low,high) = V.foldl'
              (\(!low,!high) (a,b,c) -> ( zipMin low $ zipMin a $ zipMin b c
                                        , zipMax high $ zipMax a $ zipMax b c ))
              (CVec3 (1/0) (1/0) (1/0), CVec3 (-1/0) (-1/0) (-1/0))
              tris
          zipMin = zipWith min
          zipMax = zipWith max

-- maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
-- maybeMin a b = min <$> a <*> b <|> a <|> b

-- TODO: no more pretty Maybe monad code :(
instance AccelerationStructure BBStructure where
    ray@(MkRay !o !d) `intersect` (MkN !tris) =
        let (posT, !numChecked, !numHit) = V.foldl'
                (\(!closest,!checked,!hit) tri -> case (ray `intersectTri` tri) of
                    Just int -> case closest of
                        Just m  -> (Just (min m int), checked+1, hit+1)
                        Nothing -> (Just int, checked+1, hit+1)
                    Nothing -> (closest, checked+1, hit))
                (Nothing, 0, 0) tris

        in case posT of
            Just t -> (Just (MkIntersection (o <+> (d .^ t)) t), numChecked, numHit)
            Nothing -> (Nothing, numChecked, numHit)

    build = MkN . V.fromList
