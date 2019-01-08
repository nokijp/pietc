module TestUtils
  ( toVector2D
  ) where

import Data.Vector (Vector)
import qualified Data.Vector.Generic as V

toVector2D :: [[a]] -> Vector (Vector a)
toVector2D = V.fromList . fmap V.fromList
