module Language.Piet.CompileOption
  ( OptimizationLevel(..)
  ) where

data OptimizationLevel = NoOptimization
                       | OptimizationLevelLow
                       | OptimizationLevelMiddle
                       | OptimizationLevelHigh
                       | SizeLevelLow
                       | SizeLevelHigh
                         deriving (Show, Eq)
