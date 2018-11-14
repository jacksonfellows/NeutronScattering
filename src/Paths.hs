module Paths
    ( writePaths
    ) where

import           Data.List   (intercalate)
import           Text.Printf (printf)

import           Linear

writePaths :: String -> V3 Double -> [[(V3 Double)]] -> IO ()
writePaths filename source paths = writeFile filename $ writeVertex source ++ writePaths' 2 paths

-- Write the paths to a string with the provided "offset"
-- TODO: also ugly
writePaths' :: Int -> [[(V3 Double)]] -> String
writePaths' _ [[]] = ""
writePaths' offset (p:ps) = writePath offset p ++ writePaths' (offset + length p) ps

writePath :: Int -> [V3 Double] -> String
writePath _ [] = ""
writePath offset path = vertices ++ line
    where
        vertices = concatMap writeVertex path
        -- all paths start with the source
        line = "l 1 " ++ (intercalate " " $ map show [offset..(offset + (length path) - 1)]) ++ "\n"

writeVertex :: V3 Double -> String
writeVertex (V3 x y z) = printf "v %f %f %f\n" x y z

