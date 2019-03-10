{-# LANGUAGE QuasiQuotes #-}

module Language.Piet.Internal.FillerSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Char
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Piet.Internal.Filler
import Test.Hspec
import TestUtils
import Text.InterpolatedString.Perl6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fillAll" $ do
    forM_
      [ ("when given an empty image", V.empty, V.empty, [])
      , ("when given a small image", smallImage, expectedFilledSmallImage, expectedSmallCoords)
      , ("when given a complex image", complexImage, expectedFilledComplexImage, expectedComplexCoords)
      , ("when given an irregular image", irregularImage, expectedFilledIrregularImage, expectedIrregularCoords)
      ] $ \(name, image, expectedFilledImage, expectedCoords) ->
        context name $ do
          let (filledImage, coords) = fillAll image
          it "fills all blocks in a given image" $ filledImage `shouldBe` expectedFilledImage
          it "returns coordinates of filled blocks" $ fmap S.fromList <$> IM.toAscList coords `shouldBe` expectedCoords

smallImage :: Vector (Vector Char)
smallImage = toVector2D [['a']]

expectedFilledSmallImage :: Vector (Vector Int)
expectedFilledSmallImage = toVector2D [[0]]

expectedSmallCoords :: [(Int, Set (Int, Int))]
expectedSmallCoords = [(0, S.fromList [(0, 0)])]

complexImage :: Vector (Vector Char)
complexImage = toVector2D $ tail $ lines $ [q|
GGGGGBrrr rrrMMM
bbbBBBBB    YYY*
bbbbRBBR  YYY**m
cccRRRRRR*****mm
  cccRRRRRRR*mm*
   cccccRg**mmm*
        grrr*GGr
 y    CC ggg   *
|]

expectedFilledComplexImage :: Vector (Vector Int)
expectedFilledComplexImage = fmap (fmap (\c -> ord c - ord 'a')) $ toVector2D $ tail $ lines $ [q|
aaaaabcccdeeefff
gggbbbbbddddhhhi
ggggjbbjddhhhkkl
mmmjjjjjjkkkkkll
nnmmmjjjjjjjkllo
nnnmmmmmjpqqlllo
nnnnnnnnrssstuuv
nwnnnnxxyzzz{{{|
|]

expectedComplexCoords :: [(Int, Set (Int, Int))]
expectedComplexCoords = [ (0, S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)])
                        , (1, S.fromList [(5, 0), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1), (5, 2), (6, 2)])
                        , (2, S.fromList [(6, 0), (7, 0), (8, 0)])
                        , (3, S.fromList [(9, 0), (8, 1), (9, 1), (10, 1), (11, 1), (8, 2), (9, 2)])
                        , (4, S.fromList [(10, 0), (11, 0), (12, 0)])
                        , (5, S.fromList [(13, 0), (14, 0), (15, 0)])
                        , (6, S.fromList [(0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2), (3, 2)])
                        , (7, S.fromList [(12, 1), (13, 1), (14, 1), (10, 2), (11, 2), (12, 2)])
                        , (8, S.fromList [(15, 1)])
                        , (9, S.fromList [(4, 2), (7, 2), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (5, 4), (6, 4), (7, 4), (8, 4), (9, 4), (10, 4), (11, 4), (8, 5)])
                        , (10, S.fromList [(13, 2), (14, 2), (9, 3), (10, 3), (11, 3), (12, 3), (13, 3), (12, 4)])
                        , (11, S.fromList [(15, 2), (14, 3), (15, 3), (13, 4), (14, 4), (12, 5), (13, 5), (14, 5)])
                        , (12, S.fromList [(0, 3), (1, 3), (2, 3), (2, 4), (3, 4), (4, 4), (3, 5), (4, 5), (5, 5), (6, 5), (7, 5)])
                        , (13, S.fromList [(0, 4), (1, 4), (0, 5), (1, 5), (2, 5), (0, 6), (1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (7, 6), (0, 7), (2, 7), (3, 7), (4, 7), (5, 7)])
                        , (14, S.fromList [(15, 4), (15, 5)])
                        , (15, S.fromList [(9, 5)])
                        , (16, S.fromList [(10, 5), (11, 5)])
                        , (17, S.fromList [(8, 6)])
                        , (18, S.fromList [(9, 6), (10, 6), (11, 6)])
                        , (19, S.fromList [(12, 6)])
                        , (20, S.fromList [(13, 6), (14, 6)])
                        , (21, S.fromList [(15, 6)])
                        , (22, S.fromList [(1, 7)])
                        , (23, S.fromList [(6, 7), (7, 7)])
                        , (24, S.fromList [(8, 7)])
                        , (25, S.fromList [(9, 7), (10, 7), (11, 7)])
                        , (26, S.fromList [(12, 7), (13, 7), (14, 7)])
                        , (27, S.fromList [(15, 7)])
                        ]

irregularImage :: Vector (Vector Char)
irregularImage = toVector2D $ tail $ lines $ [q|
abaa
cca
c
cccaaaa
|]

expectedFilledIrregularImage :: Vector (Vector Int)
expectedFilledIrregularImage = fmap (fmap (\c -> ord c - ord 'a')) $ toVector2D $ tail $ lines $ [q|
abcc
ddc
d
dddeeee
|]

expectedIrregularCoords :: [(Int, Set (Int, Int))]
expectedIrregularCoords = [ (0, S.fromList [(0, 0)])
                          , (1, S.fromList [(1, 0)])
                          , (2, S.fromList [(2, 0), (3, 0), (2, 1)])
                          , (3, S.fromList [(0, 1), (1, 1), (0, 2), (0, 3), (1, 3), (2, 3)])
                          , (4, S.fromList [(3, 3), (4, 3), (5, 3), (6, 3)])
                          ]
