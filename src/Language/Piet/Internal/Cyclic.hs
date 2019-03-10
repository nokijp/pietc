module Language.Piet.Internal.Cyclic
  ( cyclicSucc
  ) where

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc x | x == maxBound = minBound
             | otherwise = succ x
