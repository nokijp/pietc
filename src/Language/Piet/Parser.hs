{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- | Functions to parse images.
module Language.Piet.Parser
  ( ParserError(..)
  , parse
  , parseFilledImage
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import Language.Piet.Codel
import Language.Piet.Internal.Filler
import Language.Piet.Syntax

data ParserError = EmptyBlockTableError  -- ^ The block table is empty.
                 | IllegalInitialColorError Codel  -- ^ The initial codel of the block table is illegal.
                 | MissingCodelIndexError Int  -- ^ A codel index in the codel table is missing.
                 | IllegalCoordinateError Int Int  -- ^ A coordinate in the codel table is missing in the image.
                   deriving (Show, Eq)

-- | Parse codels into a 'SyntaxGraph'.
parse :: MonadError ParserError m => Vector (Vector Codel) -> m SyntaxGraph
parse image = let (indices, positionTable) = fillAll image in parseFilledImage (V.zipWith V.zip image indices, positionTable)

-- | Parse a filled image which is returned by 'fillAll' into a 'SyntaxGraph'.
parseFilledImage :: MonadError ParserError m => (Vector (Vector (Codel, Int)), IntMap [(Int, Int)]) -> m SyntaxGraph
parseFilledImage (codelTable, blockTable) = parse' where
  parse' :: MonadError ParserError m => m SyntaxGraph
  parse' = do
    when (IM.null blockTable) $ throwError EmptyBlockTableError
    let initialIndex = minimum $ IM.keys blockTable
    (initialX, initialY) <- justOrThrow (MissingCodelIndexError initialIndex) $ blockTable IM.!? initialIndex >>= listToMaybe
    (initialCodel, _) <- justOrThrow (IllegalCoordinateError initialX initialY) $ codelTable V.!? initialY >>= (V.!? initialX)
    unless (isAchromaticCodel initialCodel) $ throwError $ IllegalInitialColorError initialCodel
    SyntaxGraph <$> execStateT (parseState initialIndex) IM.empty

  parseState :: (MonadError ParserError m, MonadState (IntMap Block) m) => Int -> m ()
  parseState blockIndex = do
    blockCoords <- justOrThrow (MissingCodelIndexError blockIndex) $ blockTable IM.!? blockIndex

    let blockSize = length blockCoords
    let nextBlockList = mapMaybe (\((dp, cc), pos) -> ((dp, cc),) <$> nextBlock codelTable dp pos blockSize) $ minMaxCoords blockCoords
    let block = Block $ M.fromList nextBlockList
    modify $ IM.insert blockIndex block

    visitedIndices <- IM.keysSet <$> get
    let nextBlockIndices = snd . snd <$> nextBlockList
    let unvisitedBlockIndices = filter (`IS.notMember` visitedIndices) nextBlockIndices
    mapM_ parseState unvisitedBlockIndices

nextBlock :: Vector (Vector (Codel, Int)) -> DirectionPointer -> (Int, Int) -> Int -> Maybe (Command, Int)
nextBlock codelTable dp (x, y) blockSize = nextBlock' where
  nextBlock' :: Maybe (Command, Int)
  nextBlock' = do
    (AchromaticCodel currentHue currentLightness, _) <- codelTable V.!? y >>= (V.!? x)
    searchNext 0 (currentHue, currentLightness) (x, y)

  searchNext :: Int -> (Hue, Lightness) -> (Int, Int) -> Maybe (Command, Int)
  searchNext step currentHueAndLightness position = do
    let (nextX, nextY) = move dp position
    (nextCodel, blockIndex) <- codelTable V.!? nextY >>= (V.!? nextX)
    case nextCodel of
      AchromaticCodel nextHue nextLightness ->
        Just (if step == 0 then command currentHueAndLightness (nextHue, nextLightness) blockSize else NoOperation, blockIndex)
      WhiteCodel -> searchNext (step + 1) currentHueAndLightness (nextX, nextY)
      BlackCodel -> Nothing

move :: DirectionPointer -> (Int, Int) -> (Int, Int)
move DPRight = first  succ
move DPDown  = second succ
move DPLeft  = first  pred
move DPUp    = second pred

command :: (Hue, Lightness) -> (Hue, Lightness) -> Int -> Command
command (currentHue, currentLightness) (nextHue, nextLightness) = cmd where
  cmd = commandConstructors V.! (hueDiff * 3 + lightnessDiff)
  hueDiff = (fromEnum nextHue - fromEnum currentHue) `mod` 6
  lightnessDiff = (fromEnum nextLightness - fromEnum currentLightness) `mod` 3

{-# ANN minMaxCoords "HLint: ignore Redundant id" #-}
{-# ANN minMaxCoords "HLint: ignore Use first" #-}
{-# ANN minMaxCoords "HLint: ignore Use second" #-}
minMaxCoords :: [(Int, Int)] -> [((DirectionPointer, CodelChooser), (Int, Int))]
minMaxCoords positions = fmap (`maximumOn` positions) <$> fs where
  fs = [ ((DPRight, CCLeft),  (id     *** negate) . id  )
       , ((DPRight, CCRight), (id     *** id    ) . id  )
       , ((DPDown,  CCLeft),  (id     *** id    ) . swap)
       , ((DPDown,  CCRight), (id     *** negate) . swap)
       , ((DPLeft,  CCLeft),  (negate *** id    ) . id  )
       , ((DPLeft,  CCRight), (negate *** negate) . id  )
       , ((DPUp,    CCLeft),  (negate *** negate) . swap)
       , ((DPUp,    CCRight), (negate *** id    ) . swap)
       ]

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (\x y -> compare (f x) (f y))

justOrThrow :: MonadError e m => e -> Maybe a -> m a
justOrThrow e = maybe (throwError e) return
