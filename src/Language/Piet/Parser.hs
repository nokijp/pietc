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
    let nextBlockList = mapMaybe (\(dpcc, pos) -> (dpcc,) <$> nextBlock codelTable dpcc pos blockSize) $ minMaxCoords blockCoords
    let block = Block $ M.fromList nextBlockList
    modify $ IM.insert blockIndex block

    visitedIndices <- IM.keysSet <$> get
    let nextBlockIndices = getBlockIndex . snd <$> nextBlockList
    let unvisitedBlockIndices = filter (`IS.notMember` visitedIndices) nextBlockIndices
    mapM_ parseState unvisitedBlockIndices

nextBlock :: Vector (Vector (Codel, Int)) -> DPCC -> (Int, Int) -> Int -> Maybe NextBlock
nextBlock codelTable dpcc (x, y) blockSize = nextBlock' where
  nextBlock' :: Maybe NextBlock
  nextBlock' = do
    (AchromaticCodel currentHue currentLightness, _) <- codelTable V.!? y >>= (V.!? x)
    searchNext 0 (currentHue, currentLightness) (x, y)

  searchNext :: Int -> (Hue, Lightness) -> (Int, Int) -> Maybe NextBlock
  searchNext step currentHueAndLightness position = do
    let (nextX, nextY) = move (getDP dpcc) position
    (nextCodel, blockIndex) <- codelTable V.!? nextY >>= (V.!? nextX)
    case nextCodel of
      AchromaticCodel nextHue nextLightness ->
        let command = if step == 0 then commandFromTransition currentHueAndLightness (nextHue, nextLightness) blockSize else NoOperation
        in Just NextBlock { getCommand = command, getDPCC = dpcc, getBlockIndex = blockIndex }
      WhiteCodel -> searchNext (step + 1) currentHueAndLightness (nextX, nextY)
      BlackCodel -> Nothing

move :: DirectionPointer -> (Int, Int) -> (Int, Int)
move DPRight = first  succ
move DPDown  = second succ
move DPLeft  = first  pred
move DPUp    = second pred

{-# ANN minMaxCoords "HLint: ignore Redundant id" #-}
{-# ANN minMaxCoords "HLint: ignore Use first" #-}
{-# ANN minMaxCoords "HLint: ignore Use second" #-}
minMaxCoords :: [(Int, Int)] -> [(DPCC, (Int, Int))]
minMaxCoords positions = fmap (`maximumOn` positions) <$> fs where
  fs = [ (DPCC { getDP = DPRight, getCC = CCLeft },  (id     *** negate) . id  )
       , (DPCC { getDP = DPRight, getCC = CCRight }, (id     *** id    ) . id  )
       , (DPCC { getDP = DPDown,  getCC = CCLeft },  (id     *** id    ) . swap)
       , (DPCC { getDP = DPDown,  getCC = CCRight }, (id     *** negate) . swap)
       , (DPCC { getDP = DPLeft,  getCC = CCLeft },  (negate *** id    ) . id  )
       , (DPCC { getDP = DPLeft,  getCC = CCRight }, (negate *** negate) . id  )
       , (DPCC { getDP = DPUp,    getCC = CCLeft },  (negate *** negate) . swap)
       , (DPCC { getDP = DPUp,    getCC = CCRight }, (negate *** id    ) . swap)
       ]

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (\x y -> compare (f x) (f y))

justOrThrow :: MonadError e m => e -> Maybe a -> m a
justOrThrow e = maybe (throwError e) return
