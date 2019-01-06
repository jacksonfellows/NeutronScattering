{-# LANGUAGE BangPatterns #-}

module NaiveAccelerationStructure
    ( NaiveStructure
    ) where

import           Control.Applicative   ((<|>))
import           Data.Vec3
import qualified Data.Vector.Unboxed   as V

import           AccelerationStructure
import           Ray
import           Triangle

newtype NaiveStructure = MkN (V.Vector Triangle)

-- maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
-- maybeMin a b = min <$> a <*> b <|> a <|> b

-- TODO: no more pretty Maybe monad code :(
instance AccelerationStructure NaiveStructure where
    ray@(MkRay !o !d) `intersect` (MkN !vecs) =
        let (posT, !numChecked, !numHit) = V.foldl
                (\(!closest,!checked,!hit) tri -> case (ray `intersectTri` tri) of
                    Just int -> case closest of
                        Just m  -> (Just (min m int), checked+1, hit+1)
                        Nothing -> (Just int, checked+1, hit+1)
                    Nothing -> (closest, checked+1, hit))
                (Nothing, 0, 0) vecs

        in case posT of
            Just t -> (Just (MkIntersection (o <+> (d .^ t)) t), numChecked, numHit)
            Nothing -> (Nothing, numChecked, numHit)

    build = MkN . V.fromList
