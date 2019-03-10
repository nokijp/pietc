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
syntaxToDOT SyntaxGraph { getInitialBlockIndex = blockIndex, getInitialDPCC = dpcc, getBlockMap = blockMap } =
  T.intercalate "\n" [ "digraph {"
                     , "  rankdir=LR"
                     , "  node [label=\"\" shape=point color=white]"
                     , "  start"
                     , "  node [label=\"\" shape=circle color=black]"
                     , startEdge "  " blockIndex dpcc
                     , T.intercalate "\n" $ dotLines "  " blockMap
                     , "}"
                     ]

startEdge :: Text -> Int -> DPCC -> Text
startEdge indent blockIndex dpcc = T.concat [ indent
                                            , "start -> "
                                            , showText blockIndex
                                            , " [label=\""
                                            , T.pack $ showDPCC dpcc
                                            , "\"]"
                                            ]

dotLines :: Text -> IntMap Block -> [Text]
dotLines indent blockMap = do
  (from, block) <- IM.toAscList blockMap
  (dpcc, nextBlock) <- M.toAscList $ nextBlockTable block
  let nextDPCC = getDPCC nextBlock
  return $ T.concat [ indent
                    , showText from
                    , " -> "
                    , showText $ getBlockIndex nextBlock
                    , " [label=\""
                    , T.pack $ showDPCC dpcc
                    , ": "
                    , T.pack $ showCommand $ getCommand nextBlock
                    , if nextDPCC /= dpcc then T.append " -> " $ T.pack $ showDPCC nextDPCC else ""
                    , "\"]"
                    ]

showText :: Show a => a -> Text
showText = T.pack . show
