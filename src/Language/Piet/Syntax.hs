module Language.Piet.Syntax
  ( DirectionPointer(..)
  , CodelChooser(..)
  , Command(..)
  , commandConstructors
  , Block(..)
  , SyntaxGraph(..)
  ) where

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V

data DirectionPointer = DPRight | DPDown | DPLeft | DPUp deriving (Show, Eq, Ord, Enum)
data CodelChooser = CCLeft | CCRight deriving (Show, Eq, Ord, Enum)
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

newtype Block = Block { nextBlockTable :: Map (DirectionPointer, CodelChooser) (Command, Int) } deriving (Show, Eq)
newtype SyntaxGraph = SyntaxGraph { getSyntaxGraph :: IntMap Block } deriving (Show, Eq)
