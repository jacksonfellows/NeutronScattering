module ArbitraryHelpers where

import           Linear.V3
import           Test.Tasty.QuickCheck

instance Arbitrary a => Arbitrary (V3 a) where
    -- TODO: there is a better way to write this
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ V3 x y z
