{-# LANGUAGE QuasiQuotes #-}

module Language.Piet.Internal.CodelSizeSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Piet.Internal.CodelSize
import Test.Hspec
import TestUtils
import Text.InterpolatedString.Perl6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "guessCodelSize" $ do
    forM_
      [ ("emptyImage", V.empty, 0)
      , ("smallestImage", smallestImage, 1)
      --, ("largeWhiteImage", largeWhiteImage, largeImageSize)
      --, ("largeCheckImage", largeCheckImage, 1)
      , ("size3Image", size3Image, 3)
      , ("size1Image", size1Image, 1)
      ] $ \(name, image, codelSize) ->
        context ("when given " ++ name) $ do
          it "return the codel size of an image" $ guessCodelSize image `shouldBe` codelSize

smallestImage :: Vector (Vector Char)
smallestImage = toVector2D [['a']]

{-
largeWhiteImage :: Vector (Vector Char)
largeWhiteImage = V.replicate largeImageSize $ V.replicate largeImageSize 'a'

largeCheckImage :: Vector (Vector Char)
largeCheckImage = V.generate largeImageSize $ \y -> V.generate largeImageSize $ \x -> if (x + y) `mod` 2 == 0 then 'a' else 'b'

largeImageSize :: Int
largeImageSize = 10000
-}

size3Image :: Vector (Vector Char)
size3Image = toVector2D $ tail $ lines $ [q|
aaabbbbbb
aaabbbbbb
aaabbbbbb
ccccccddd
ccccccddd
ccccccddd
ccccccddd
ccccccddd
ccccccddd
|]

size1Image :: Vector (Vector Char)
size1Image = toVector2D $ tail $ lines $ [q|
aaabb
aaabb
aaabb
cccdd
cccdd
|]
