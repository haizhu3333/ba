module Utils (
    Direction(..), mapFirst
) where

import Control.Applicative (Alternative)
import Data.Monoid (Alt(..))

data Direction = N | S | E | W
    deriving (Eq, Ord, Show)

mapFirst :: (Alternative m, Foldable f) => (a -> m b) -> f a -> m b
mapFirst f xs = getAlt $ foldMap (Alt . f) xs
