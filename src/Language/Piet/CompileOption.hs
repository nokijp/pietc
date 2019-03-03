module Language.Piet.CompileOption
  ( OptimizationLevel(..)
  ) where

-- | The optimization level to optimize assembly code.
data OptimizationLevel = NoOptimization  -- ^ -O0
                       | OptimizationLevelLow  -- ^ -O1
                       | OptimizationLevelMiddle  -- ^ -O2
                       | OptimizationLevelHigh  -- ^ -O3
                       | SizeLevelLow  -- ^ -Os
                       | SizeLevelHigh  -- ^ -Oz
                         deriving (Show, Eq)
