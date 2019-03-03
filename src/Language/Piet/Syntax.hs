-- | A representation of the syntax of Piet.
module Language.Piet.Syntax
  ( Block(..)
  , SyntaxGraph(..)
  , DirectionPointer(..)
  , CodelChooser(..)
  , Command(..)
  , commandConstructors
  ) where

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V

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
