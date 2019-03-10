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
--
-- When the step is stuck, this returns 'Nothing'.
slideOnWhiteBlock :: Vector (Vector (Codel, Int)) -> (Int, Int) -> DPCC -> Maybe NextBlock
slideOnWhiteBlock image initialPosition initialDPCC = result where
  result = (`evalState` S.empty) $ fmap (either id $ error "unreachable") . runExceptT $ slideOnWhiteBlockState initialPosition initialDPCC

  slideOnWhiteBlockState :: ( MonadState (Set ((Int, Int), DPCC)) m
                            , MonadError (Maybe NextBlock) m
                            )
                         => (Int, Int) -> DPCC -> m ()
  slideOnWhiteBlockState position dpcc = do
    (nextCodel, nextIndex, nextPosition, nextDPCC) <- maybe (throwError Nothing) return $ next position dpcc
    when (nextCodel /= WhiteCodel) $ throwError $ Just NextBlock { getCommand = NoOperation
                                                                 , getDPCC = nextDPCC
                                                                 , getBlockIndex = nextIndex
                                                                 }

    visited <- get
    let nextCodelDPCC = (nextPosition, nextDPCC)
    when (S.member nextCodelDPCC visited) $ throwError Nothing
    modify $ S.insert nextCodelDPCC

    slideOnWhiteBlockState nextPosition nextDPCC

  next :: (Int, Int) -> DPCC -> Maybe (Codel, Int, (Int, Int), DPCC)
  next position dpcc = listToMaybe $ do
    nextDPCC <- take 4 $ iterate succDPCC dpcc
    let nextPosition = move (getDP nextDPCC) position
    (nextCodel, nextIndex) <- maybeToList $ getNonBlackCodel nextPosition
    return (nextCodel, nextIndex, nextPosition, nextDPCC)

  getNonBlackCodel :: (Int, Int) -> Maybe (Codel, Int)
  getNonBlackCodel (x, y) = do
    (codel, index) <- image V.!? y >>= (V.!? x)
    guard $ codel /= BlackCodel
    return (codel, index)

succDPCC :: DPCC -> DPCC
succDPCC dpcc = DPCC { getDP = cyclicSucc (getDP dpcc), getCC = cyclicSucc (getCC dpcc) }
