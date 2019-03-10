-- | The types for codels.
module Language.Piet.Codel
  ( Codel(..)
  , Hue(..)
  , Lightness(..)
  ) where

data Codel = AchromaticCodel Hue Lightness | WhiteCodel | BlackCodel deriving (Show, Eq, Ord)
data Hue = Red | Yellow | Green | Cyan | Blue | Magenta deriving (Show, Eq, Ord, Enum)
data Lightness = Light | Normal | Dark deriving (Show, Eq, Ord, Enum)
