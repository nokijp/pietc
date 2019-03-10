{-# LANGUAGE OverloadedStrings #-}

-- | A visualization tool for 'SyntaxGraph'.
module Language.Piet.SyntaxVisualizer
  ( syntaxToDOT
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Piet.Syntax

-- | Convert a 'SyntaxGraph' to a DOT script, which describes graphs.
--
-- By using tools such as Graphviz, DOT graphs can be converted into images.
syntaxToDOT :: SyntaxGraph -> Text
syntaxToDOT graph = T.intercalate "\n" [ "digraph {"
                                       , "  rankdir=LR"
                                       , "  node [label=\"\" shape=point color=white]"
                                       , "  start"
                                       , "  node [label=\"\" shape=circle color=black]"
                                       , "  start -> 0"
                                       , T.intercalate "\n" $ dotLines "  " graph
                                       , "}"
                                       ]

dotLines :: Text -> SyntaxGraph -> [Text]
dotLines indent graph = do
  (from, block) <- IM.toAscList $ (\(SyntaxGraph _ _ x) -> x) graph  -- FIXME
  (dpcc, nextBlock) <- M.toAscList $ nextBlockTable block
  let nextDPCC = getDPCC nextBlock
  return $ T.concat [ indent
                    , showText from
                    , " -> "
                    , showText $ getBlockIndex nextBlock
                    , " [label=\""
                    , T.pack $ showDPCC dpcc
                    , if nextDPCC /= dpcc then T.append " -> " $ showText nextDPCC else ""
                    , ": "
                    , T.pack $ showCommand $ getCommand nextBlock
                    , "\"]"
                    ]

showText :: Show a => a -> Text
showText = T.pack . show
