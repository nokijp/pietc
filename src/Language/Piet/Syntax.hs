-- | A representation of the syntax of Piet.
module Language.Piet.Syntax
  ( Block(..)
  , SyntaxGraph(..)
  , DirectionPointer(..)
  , CodelChooser(..)
  , Command(..)
  , commandFromTransition
  , showCommand
  , showDPCC
  ) where

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import Language.Piet.Codel

-- | A representation of a codel block.
--
-- 'Block' is a map to a pair of
-- a command which will be executed when moving into the next codel block
-- and an index of the next codel block.
newtype Block = Block { nextBlockTable :: Map (DirectionPointer, CodelChooser) (Command, Int) } deriving (Show, Eq)

-- | A representation of the syntax of Piet.
--
-- 'SyntaxGraph' has a graph structure whose nodes represent codel blocks and edges represent the next steps.
newtype SyntaxGraph = SyntaxGraph { getSyntaxGraph :: IntMap Block } deriving (Show, Eq)

data DirectionPointer = DPRight | DPDown | DPLeft | DPUp deriving (Show, Eq, Ord, Enum, Bounded)
data CodelChooser = CCLeft | CCRight deriving (Show, Eq, Ord, Enum, Bounded)

data Command = NoOperation
             | Push Int
             | Pop
             | Add
             | Subtract
             | Multiply
             | Divide
             | Mod
             | Not
             | Greater
             | Pointer
             | Switch
             | Duplicate
             | Roll
             | InNumber
             | InChar
             | OutNumber
             | OutChar
             deriving (Show, Eq)

commandFromTransition :: (Hue, Lightness) -> (Hue, Lightness) -> Int -> Command
commandFromTransition (currentHue, currentLightness) (nextHue, nextLightness) = cmd where
  cmd = commandConstructors V.! (hueDiff * 3 + lightnessDiff)
  hueDiff = (fromEnum nextHue - fromEnum currentHue) `mod` 6
  lightnessDiff = (fromEnum nextLightness - fromEnum currentLightness) `mod` 3

commandConstructors :: Vector (Int -> Command)
commandConstructors = V.fromList [ const NoOperation
                                 , Push
                                 , const Pop
                                 , const Add
                                 , const Subtract
                                 , const Multiply
                                 , const Divide
                                 , const Mod
                                 , const Not
                                 , const Greater
                                 , const Pointer
                                 , const Switch
                                 , const Duplicate
                                 , const Roll
                                 , const InNumber
                                 , const InChar
                                 , const OutNumber
                                 , const OutChar
                                 ]

showCommand :: Command -> String
showCommand NoOperation = "noop"
showCommand (Push n)    = "push " ++ show n
showCommand Pop         = "pop"
showCommand Add         = "add"
showCommand Subtract    = "subtract"
showCommand Multiply    = "multiply"
showCommand Divide      = "divide"
showCommand Mod         = "mod"
showCommand Not         = "not"
showCommand Greater     = "greater"
showCommand Pointer     = "pointer"
showCommand Switch      = "switch"
showCommand Duplicate   = "duplicate"
showCommand Roll        = "roll"
showCommand InNumber    = "in (number)"
showCommand InChar      = "in (char)"
showCommand OutNumber   = "out (number)"
showCommand OutChar     = "out (char)"

showDPCC :: (DirectionPointer, CodelChooser) -> String
showDPCC (dp, cc) = [charDP dp, charCC cc] where
  charDP DPRight = 'r'
  charDP DPDown  = 'd'
  charDP DPLeft  = 'l'
  charDP DPUp    = 'u'
  charCC CCLeft  = 'l'
  charCC CCRight = 'r'
