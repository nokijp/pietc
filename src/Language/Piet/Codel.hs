module Language.Piet.Codel
  ( Codel(..)
  , Hue(..)
  , Lightness(..)
  , isAchromaticCodel
  ) where

data Codel = AchromaticCodel Hue Lightness | WhiteCodel | BlackCodel deriving (Show, Eq)
data Hue = Red | Yellow | Green | Cyan | Blue | Magenta deriving (Show, Eq, Ord, Enum)
data Lightness = Light | Normal | Dark deriving (Show, Eq, Ord, Enum)

isAchromaticCodel :: Codel -> Bool
isAchromaticCodel (AchromaticCodel _ _) = True
isAchromaticCodel WhiteCodel = False
isAchromaticCodel BlackCodel = False
