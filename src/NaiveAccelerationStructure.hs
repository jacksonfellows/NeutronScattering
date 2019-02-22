{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NaiveAccelerationStructure
    ( NaiveStructure
    ) where

import           Control.Applicative ((<|>))
import qualified Data.Vector         as V
import           Prelude             hiding (zipWith)

import           AABB
import           Intersect
import           Ray

newtype NaiveStructure i n = MkN (V.Vector (i n))

instance IntersectionPrim i n => AccelerationStructure NaiveStructure i n where
    (!ray) `intersect` (MkN !prims) = V.foldl1' maybeMinOnFst $ V.map
        (\p -> (,) <$> ray `intersectPrim` p <*> return p) prims
        where maybeMinOnFst a b = minFst <$> a <*> b <|> a <|> b
              minFst a b = if fst a < fst b then a else b
    construct = MkN . V.fromList
    buildAABB (MkN !prims) = V.foldl1' union $ V.map buildAABBPrim prims
