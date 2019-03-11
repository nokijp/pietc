{-# LANGUAGE OverloadedStrings #-}

-- | A visualization tool for 'SyntaxGraph'.
module Language.Piet.SyntaxVisualizer
  ( syntaxToDOT
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Piet.Syntax

-- | Convert a 'SyntaxGraph' to a DOT script, which describes graphs.
--
-- By using tools such as Graphviz, DOT graphs can be converted into images.
syntaxToDOT :: SyntaxGraph -> Text
syntaxToDOT EmptySyntaxGraph = "digraph {}"
syntaxToDOT (SyntaxGraph blockIndex dpcc blockMap) =
  T.intercalate "\n" [ "digraph {"
                     , "  rankdir=LR"
                     , "  start [label=\"\" shape=point color=white]"
                     , "  node [label=\"\" shape=circle color=black]"
                     , startEdge blockIndex dpcc
                     , T.intercalate "\n" $ dotLines blockMap
                     , "}"
                     ]

startEdge :: Int -> DPCC -> Text
startEdge blockIndex dpcc =
  T.concat [ "  start -> "
           , showText blockIndex
           , " [label=\""
           , T.pack $ showDPCC dpcc
           , "\"]"
           ]

dotLines :: IntMap Block -> [Text]
dotLines blockMap = do
  (from, block) <- IM.toAscList blockMap
  (dpcc, nextBlock) <- M.toAscList $ nextBlockTable block
  dotBlockLines from dpcc nextBlock

dotBlockLines :: Int -> DPCC -> NextBlock -> [Text]
dotBlockLines from fromDPCC (NextBlock command toDPCC nextBlockIndex) =
  [ T.concat [ "  "
             , showText from
             , " -> "
             , showText nextBlockIndex
             , " [label=\""
             , T.pack $ showDPCC fromDPCC
             , ": "
             , T.pack $ showCommand command
             , if toDPCC /= fromDPCC then T.append " -> " $ T.pack $ showDPCC toDPCC else ""
             , "\"]"
             ]
  ]
dotBlockLines _ _ ExitProgram = undefined  -- FIXME

showText :: Show a => a -> Text
showText = T.pack . show
