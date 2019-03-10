{-# LANGUAGE QuasiQuotes #-}

module Language.Piet.Internal.WhiteCodelSliderSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Piet.Codel
import Language.Piet.Internal.WhiteCodelSlider
import Language.Piet.Syntax
import SyntaxTestHelper
import Test.Hspec
import TestUtils
import Text.InterpolatedString.Perl6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "slideOnWhiteBlock" $ do
    forM_
      [ ("when given a single codel image", singleCodelImage, (0, 0), rl, Nothing)
      , ("when given a image which has a loop", oneLoopImage, (1, 1), rl, Just $ NextBlock NoOperation ur 2)
      , ("when given a image which has a loop", oneLoopImage, (1, 1), rr, Just $ NextBlock NoOperation ul 2)
      , ("-------------1", gammaImage, (1, 1), rl, Just $ NextBlock NoOperation rl 1)
      , ("-------------2", gammaImage, (1, 4), rl, Just $ NextBlock NoOperation ll 0)
      , ("-------------3", spiralImage, (1, 1), rl, Just $ NextBlock NoOperation rl 4)
      , ("-------------4", stuckImage1, (1, 1), rl, Nothing)
      , ("-------------5", stuckImage2, (1, 1), rl, Nothing)
      , ("-------------6", stuckImage3, (1, 1), rl, Nothing)
      ] $ \(name, image, initialPosition, initialDPCC, expected) ->
        context name $
          it "slide and return the next codel" $
            slideOnWhiteBlock image initialPosition initialDPCC `shouldBe` expected

singleCodelImage :: Vector (Vector (Codel, Int))
singleCodelImage = V.singleton $ V.singleton (WhiteCodel, 0)

oneLoopImage :: Vector (Vector (Codel, Int))
oneLoopImage = V.map (V.map f) $ toVector2D $ tail $ lines $ [q|
0rgb11111111
y          1
y          1
y          1
y          1
y2         1
|] where
  f '0' = (BlackCodel, 0)
  f 'r' = (AchromaticCodel Red Normal, 1)
  f 'g' = (AchromaticCodel Green Normal, 2)
  f 'b' = (AchromaticCodel Blue Normal, 3)
  f '1' = (BlackCodel, 4)
  f 'y' = (AchromaticCodel Yellow Normal, 5)
  f ' ' = (WhiteCodel, 6)
  f '2' = (BlackCodel, 7)
  f _   = undefined

gammaImage :: Vector (Vector (Codel, Int))
gammaImage = V.map (V.map f) $ toVector2D $ tail $ lines $ [q|
r          g
r       0  b
r          c
r          m
r     1    y
|] where
  f 'r' = (AchromaticCodel Red Normal, 0)
  f 'g' = (AchromaticCodel Green Normal, 1)
  f 'b' = (AchromaticCodel Blue Normal, 2)
  f 'c' = (AchromaticCodel Cyan Normal, 3)
  f 'm' = (AchromaticCodel Magenta Normal, 4)
  f 'y' = (AchromaticCodel Yellow Normal, 5)
  f ' ' = (WhiteCodel, 6)
  f '0' = (BlackCodel, 7)
  f '1' = (BlackCodel, 8)
  f _   = undefined

spiralImage :: Vector (Vector (Codel, Int))
spiralImage = V.map (V.map f) $ toVector2D $ tail $ lines $ [q|
rrrrrrrrrrrg
y         0g
y 3        g
y       4  g
y   7      g
y     c    g
y  6       g
y      5   g
y2         g
y        1 g
ybbbbbbbbbbb
|] where
  f 'r' = (AchromaticCodel Red Normal, 0)
  f 'g' = (AchromaticCodel Green Normal, 1)
  f 'b' = (AchromaticCodel Blue Normal, 2)
  f 'y' = (AchromaticCodel Yellow Normal, 3)
  f 'c' = (AchromaticCodel Cyan Normal, 4)
  f ' ' = (WhiteCodel, 5)
  f '0' = (BlackCodel, 6)
  f '1' = (BlackCodel, 7)
  f '2' = (BlackCodel, 8)
  f '3' = (BlackCodel, 9)
  f '4' = (BlackCodel, 10)
  f '5' = (BlackCodel, 11)
  f '6' = (BlackCodel, 12)
  f '7' = (BlackCodel, 13)
  f _   = undefined

stuckImage1 :: Vector (Vector (Codel, Int))
stuckImage1 = V.map (V.map f) $ toVector2D $ tail $ lines $ [q|
0rgb11111111
y          1
y          1
y          1
y          1
y       2  1
|] where
  f '0' = (BlackCodel, 0)
  f 'r' = (AchromaticCodel Red Normal, 1)
  f 'g' = (AchromaticCodel Green Normal, 2)
  f 'b' = (AchromaticCodel Blue Normal, 3)
  f '1' = (BlackCodel, 4)
  f 'y' = (AchromaticCodel Yellow Normal, 5)
  f ' ' = (WhiteCodel, 6)
  f '2' = (BlackCodel, 7)
  f _   = undefined

stuckImage2 :: Vector (Vector (Codel, Int))
stuckImage2 = V.map (V.map f) $ toVector2D $ tail $ lines $ [q|
0rgb11111111
y          1
y          1
y          1
y          1
y        2 1
|] where
  f '0' = (BlackCodel, 0)
  f 'r' = (AchromaticCodel Red Normal, 1)
  f 'g' = (AchromaticCodel Green Normal, 2)
  f 'b' = (AchromaticCodel Blue Normal, 3)
  f '1' = (BlackCodel, 4)
  f 'y' = (AchromaticCodel Yellow Normal, 5)
  f ' ' = (WhiteCodel, 6)
  f '2' = (BlackCodel, 7)
  f _   = undefined

stuckImage3 :: Vector (Vector (Codel, Int))
stuckImage3 = V.map (V.map f) $ toVector2D $ tail $ lines $ [q|
00gb11111111
y          1
y          1
y          1
y          1
2          1
|] where
  f '0' = (BlackCodel, 0)
  f 'g' = (AchromaticCodel Green Normal, 1)
  f 'b' = (AchromaticCodel Blue Normal, 2)
  f '1' = (BlackCodel, 3)
  f 'y' = (AchromaticCodel Yellow Normal, 4)
  f ' ' = (WhiteCodel, 5)
  f '2' = (BlackCodel, 6)
  f _   = undefined
