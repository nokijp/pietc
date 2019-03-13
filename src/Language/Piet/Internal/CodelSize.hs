module Language.Piet.Internal.CodelSize
  ( guessCodelSize
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Guess the codel size of a given image.
--
-- The computational complexity of this function is about /O(n^2)/,
-- hence this takes a long time when given a large image.
guessCodelSize :: Eq a => Vector (Vector a) -> Int
guessCodelSize image = if height == 0 || width == 0 then 0 else codelSize where
  width = V.length $ V.head image
  height = V.length image
  codelSize = horizontalGCD `gcd` verticalGCD
  transposedImage = V.generate width $ \y -> V.generate height $ \x -> image V.! x V.! y
  horizontalGCD = V.foldl1' gcd $ V.generate height (groupGCD . (image V.!))
  verticalGCD = V.foldl1' gcd $ V.generate width (groupGCD . (transposedImage V.!))

groupGCD :: Eq a => Vector a -> Int
groupGCD xs | V.null xs = 0
            | otherwise = n `gcd` groupGCD nextXs where
  (n, nextXs) = countSame 0 (V.head xs) xs
  countSame acc y ys | not (V.null ys) && y == V.head ys = countSame (acc + 1) y (V.tail ys)
                     | otherwise = (acc, ys)
