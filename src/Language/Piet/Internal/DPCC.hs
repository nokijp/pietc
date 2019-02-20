module Language.Piet.Internal.DPCC
  ( dpccsToBackwardDPCCTable
  ) where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Exts
import Language.Piet.Syntax

type DPCC = (DirectionPointer, CodelChooser)

dpccsToBackwardDPCCTable :: [DPCC] -> Map DPCC [DPCC]
dpccsToBackwardDPCCTable [] = M.empty
dpccsToBackwardDPCCTable possibleDPCCs = M.fromList $ nearestTableToBackwardTable $ nearestDPCCTable possibleDPCCs where
  nearestTableToBackwardTable :: Ord b => [(a, b)] -> [(b, [a])]
  nearestTableToBackwardTable = fmap (snd . head &&& fmap fst) . groupWith snd

nearestDPCCTable :: [DPCC] -> [(DPCC, DPCC)]
nearestDPCCTable possibleDPCCs = (id &&& nearestDPCC) <$> allDPCCs where
  nearestDPCC (dp, cc) =
    let
      nearestDP = nearestDPTable M.! dp
      nearestCC = toEnum $ (fromEnum cc + fromEnum nearestDP - fromEnum dp) `mod` 2
    in if S.member (nearestDP, nearestCC) possibleDPCCSet
       then (nearestDP, nearestCC)
       else (nearestDP, cyclicSucc nearestCC)

  nearestDPTable :: Map DirectionPointer DirectionPointer
  nearestDPTable = M.fromList $ go reversedAllDPs (cycle reversedPossibleDPs) (last reversedPossibleDPs) where
    go [] _ _ = []
    go (currentDP : currentDPs) (nextDP : nextDPs) dp | currentDP == nextDP = (currentDP, nextDP) : go currentDPs nextDPs nextDP
                                                      | otherwise = (currentDP, dp) : go currentDPs (nextDP : nextDPs) dp
    go _ [] _ = error "unreachable"
    reversedPossibleDPs = S.toDescList possibleDPSet
    reversedAllDPs = reverse [minBound .. maxBound]

  allDPCCs = (,) <$> [minBound .. maxBound] <*> [minBound .. maxBound]
  possibleDPCCSet = S.fromList possibleDPCCs
  possibleDPSet = S.map fst possibleDPCCSet

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc x | x == maxBound = minBound
             | otherwise = succ x
