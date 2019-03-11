{-# LANGUAGE OverloadedStrings #-}

-- | A visualization tool for 'SyntaxGraph'.
module Language.Piet.SyntaxVisualizer
  ( syntaxToDOT
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder
import Language.Piet.Syntax

-- | Convert a 'SyntaxGraph' to a DOT script, which describes graphs.
--
-- By using tools such as Graphviz, DOT graphs can be converted into images.
syntaxToDOT :: SyntaxGraph -> Text
syntaxToDOT EmptySyntaxGraph = "digraph {}"
syntaxToDOT (SyntaxGraph blockIndex dpcc blockMap) =
  toLazyText $  "digraph {\n"
             <> "  rankdir=LR\n"
             <> "  start [label=\"\" shape=point color=white]\n"
             <> "  node [label=\"\" shape=circle color=black]\n"
             <> startEdge blockIndex dpcc
             <> mconcat (blocks blockMap)
             <> "}"

startEdge :: Int -> DPCC -> Builder
startEdge blockIndex dpcc =
  "  start -> " <> showBuilder blockIndex <> " [label=\"" <> fromString (showDPCC dpcc) <> "\"]\n"

blocks :: IntMap Block -> [Builder]
blocks blockMap = do
  (from, block) <- IM.toAscList blockMap
  let dpccAndNextBlock = M.toAscList $ nextBlockTable block
  return $ if null dpccAndNextBlock then emptyBlock from else nonemptyBlock from dpccAndNextBlock

nonemptyBlock :: Int -> [(DPCC, NextBlock)] -> Builder
nonemptyBlock from dpccAndNextBlock = nodeLine <> edgeLines where
  hasExit = any ((== ExitProgram) . snd) dpccAndNextBlock
  nodeLine = if hasExit then exitEdge from else ""
  edgeLines = foldMap (uncurry $ nextBlockEdge from) dpccAndNextBlock

emptyBlock :: Int -> Builder
emptyBlock from = exitEdge from <> "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"\"]\n"

exitEdge :: Int -> Builder
exitEdge from = "  exit" <> showBuilder from <> " [label=\"\" shape=point color=white]\n"

nextBlockEdge :: Int -> DPCC -> NextBlock -> Builder
nextBlockEdge from fromDPCC (NextBlock command toDPCC nextBlockIndex) =
  let nextDPCCText = if toDPCC /= fromDPCC then " -> " <> fromString (showDPCC toDPCC) else ""
  in "  " <> showBuilder from <> " -> " <> showBuilder nextBlockIndex
       <> " [label=\"" <> fromString (showDPCC fromDPCC) <> ": " <> fromString (showCommand command) <> nextDPCCText <> "\"]\n"
nextBlockEdge from fromDPCC ExitProgram =
  "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"" <> fromString (showDPCC fromDPCC) <> "\"]\n"

showBuilder :: Show a => a -> Builder
showBuilder = fromString . show
