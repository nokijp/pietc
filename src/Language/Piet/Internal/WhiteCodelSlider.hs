{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Language.Piet.Internal.WhiteCodelSlider
  ( slideOnWhiteBlock
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Piet.Codel
import Language.Piet.Internal.Cyclic
import Language.Piet.Internal.Position
import Language.Piet.Syntax

-- | Move on white codels and return 'NextBlock'.
slideOnWhiteBlock :: Vector (Vector (Codel, Int)) -> (Int, Int) -> DPCC -> NextBlock
slideOnWhiteBlock image initialPosition initialDPCC = result where
  result = (`evalState` S.empty) $ fmap (either id $ error "unreachable") . runExceptT $ slideOnWhiteBlockState initialPosition initialDPCC

  slideOnWhiteBlockState :: ( MonadState (Set ((Int, Int), DPCC)) m
                            , MonadError NextBlock m
                            )
                         => (Int, Int) -> DPCC -> m ()
  slideOnWhiteBlockState position dpcc = do
    (nextCodel, nextIndex, nextPosition, nextDPCC) <- maybe (throwError ExitProgram) return $ next position dpcc
    when (nextCodel /= WhiteCodel) $ throwError $ NextBlock NoOperation nextDPCC nextIndex

    visited <- get
    let nextCodelDPCC = (nextPosition, nextDPCC)
    when (S.member nextCodelDPCC visited) $ throwError ExitProgram
    modify $ S.insert nextCodelDPCC

    slideOnWhiteBlockState nextPosition nextDPCC

  next :: (Int, Int) -> DPCC -> Maybe (Codel, Int, (Int, Int), DPCC)
  next position dpcc = listToMaybe $ do
    nextDPCC@(DPCC nextDP _) <- take 4 $ iterate succDPCC dpcc
    let nextPosition = move nextDP position
    (nextCodel, nextIndex) <- maybeToList $ getNonBlackCodel nextPosition
    return (nextCodel, nextIndex, nextPosition, nextDPCC)

  getNonBlackCodel :: (Int, Int) -> Maybe (Codel, Int)
  getNonBlackCodel (x, y) = do
    (codel, index) <- image V.!? y >>= (V.!? x)
    guard $ codel /= BlackCodel
    return (codel, index)

succDPCC :: DPCC -> DPCC
succDPCC (DPCC dp cc) = DPCC (cyclicSucc dp) (cyclicSucc cc)
