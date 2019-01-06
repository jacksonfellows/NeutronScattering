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

maybeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
maybeMin a b = min <$> a <*> b <|> a <|> b

instance AccelerationStructure NaiveStructure where
    ray@(MkRay !o !d) `intersect` (MkN !vecs) = do
        t <- V.foldl (\min tri -> maybeMin min (ray `intersectTri` tri)) Nothing vecs
        return $ MkIntersection (o <+> (d .^ t)) t

    build = MkN . V.fromList
