{-# LANGUAGE QuasiQuotes #-}

module Language.Piet.SyntaxVisualizerSpec
  ( main
  , spec
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text (Text)
import Language.Piet.Syntax
import Language.Piet.SyntaxVisualizer
import Test.Hspec
import Text.InterpolatedString.Perl6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "syntaxToDOT" $
    it "convert a syntax graph to a DOT script" $ syntaxToDOT complexGraph `shouldBe` complexDOT

complexGraph :: SyntaxGraph
complexGraph = SyntaxGraph $ IM.fromList [ ( 0
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock Pop dpcc 1)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock Pop dpcc 1)
                                                                , let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock Pop dpcc 1)
                                                                , let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock (Push 5) dpcc 6)
                                                                ]
                                           )
                                         , ( 1
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock NoOperation dpcc 7)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock NoOperation dpcc 7)
                                                                , let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock Divide dpcc 9)
                                                                , let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock Divide dpcc 9)
                                                                , let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock Pop dpcc 6)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock Pop dpcc 6)
                                                                ]
                                           )
                                         , ( 2
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock NoOperation dpcc 4)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock NoOperation dpcc 4)
                                                                , let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock NoOperation dpcc 9)
                                                                , let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock Roll dpcc 1)
                                                                , let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock Roll dpcc 1)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock Roll dpcc 1)
                                                                ]
                                           )
                                         , ( 4
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock OutChar dpcc 5)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock OutChar dpcc 5)
                                                                , let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock Subtract dpcc 7)
                                                                , let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock NoOperation dpcc 7)
                                                                , let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock NoOperation dpcc 2)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock NoOperation dpcc 2)
                                                                ]
                                           )
                                         , ( 5
                                           , Block $ M.fromList [ let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock Not dpcc 7)
                                                                , let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock Subtract dpcc 4)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock Subtract dpcc 4)
                                                                ]
                                           )
                                         , ( 6
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock Mod dpcc 9)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock Mod dpcc 9)
                                                                , let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock Mod dpcc 9)
                                                                , let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock InChar dpcc 12)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock Pop dpcc 0)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock Pop dpcc 0)
                                                                ]
                                           )
                                         , ( 7
                                           , Block $ M.fromList [ let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock NoOperation dpcc 9)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock NoOperation dpcc 9)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock OutChar dpcc 4)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock Roll dpcc 5)
                                                                ]
                                           )
                                         , ( 9
                                           , Block $ M.fromList [ let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock (Push 16) dpcc 17)
                                                                , let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock (Push 16) dpcc 17)
                                                                , let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock Switch dpcc 12)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock Switch dpcc 12)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock Duplicate dpcc 1)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock Duplicate dpcc 1)
                                                                ]
                                           )
                                         , ( 12
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock Pointer dpcc 9)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock Pointer dpcc 9)
                                                                , let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock NoOperation dpcc 23)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock Add dpcc 6)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock Add dpcc 6)
                                                                ]
                                           )
                                         , ( 15
                                           , Block $ M.fromList [ let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock Duplicate dpcc 18)
                                                                , let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock Duplicate dpcc 18)
                                                                , let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock Roll dpcc 9)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock Roll dpcc 9)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock Roll dpcc 9)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock Roll dpcc 9)
                                                                ]
                                           )
                                         , ( 17
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock (Push 1) dpcc 18)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock (Push 1) dpcc 18)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock Pop dpcc 9)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock Pop dpcc 9)
                                                                ]
                                           )
                                         , ( 18
                                           , Block $ M.fromList [ let dpcc = DPCC DPDown CCLeft in (dpcc, NextBlock Divide dpcc 25)
                                                                , let dpcc = DPCC DPDown CCRight in (dpcc, NextBlock Divide dpcc 25)
                                                                , let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock Pop dpcc 17)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock Pop dpcc 17)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock Divide dpcc 15)
                                                                ]
                                           )
                                         , ( 22
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock NoOperation dpcc 23)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock NoOperation dpcc 23)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock NoOperation dpcc 12)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock NoOperation dpcc 12)
                                                                ]
                                           )
                                         , ( 23
                                           , Block $ M.fromList [ let dpcc = DPCC DPRight CCLeft in (dpcc, NextBlock NoOperation dpcc 25)
                                                                , let dpcc = DPCC DPRight CCRight in (dpcc, NextBlock NoOperation dpcc 25)
                                                                , let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock NoOperation dpcc 22)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock NoOperation dpcc 22)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock NoOperation dpcc 12)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock NoOperation dpcc 12)
                                                                ]
                                           )
                                         , ( 25
                                           , Block $ M.fromList [ let dpcc = DPCC DPLeft CCLeft in (dpcc, NextBlock NoOperation dpcc 23)
                                                                , let dpcc = DPCC DPLeft CCRight in (dpcc, NextBlock NoOperation dpcc 23)
                                                                , let dpcc = DPCC DPUp CCLeft in (dpcc, NextBlock Duplicate dpcc 18)
                                                                , let dpcc = DPCC DPUp CCRight in (dpcc, NextBlock Duplicate dpcc 18)
                                                                ]
                                           )
                                         ]

complexDOT :: Text
complexDOT = [q|digraph {
  rankdir=LR
  node [label="" shape=point color=white]
  start
  node [label="" shape=circle color=black]
  start -> 0
  0 -> 1 [label="rl: pop"]
  0 -> 1 [label="rr: pop"]
  0 -> 1 [label="dl: pop"]
  0 -> 6 [label="dr: push 5"]
  1 -> 7 [label="rl: noop"]
  1 -> 7 [label="rr: noop"]
  1 -> 9 [label="dl: divide"]
  1 -> 9 [label="dr: divide"]
  1 -> 6 [label="ll: pop"]
  1 -> 6 [label="lr: pop"]
  2 -> 4 [label="rl: noop"]
  2 -> 4 [label="rr: noop"]
  2 -> 9 [label="dl: noop"]
  2 -> 1 [label="dr: roll"]
  2 -> 1 [label="ll: roll"]
  2 -> 1 [label="lr: roll"]
  4 -> 5 [label="rl: out (char)"]
  4 -> 5 [label="rr: out (char)"]
  4 -> 7 [label="dl: subtract"]
  4 -> 7 [label="dr: noop"]
  4 -> 2 [label="ll: noop"]
  4 -> 2 [label="lr: noop"]
  5 -> 7 [label="dr: not"]
  5 -> 4 [label="ll: subtract"]
  5 -> 4 [label="lr: subtract"]
  6 -> 9 [label="rl: mod"]
  6 -> 9 [label="rr: mod"]
  6 -> 9 [label="dl: mod"]
  6 -> 12 [label="dr: in (char)"]
  6 -> 0 [label="ul: pop"]
  6 -> 0 [label="ur: pop"]
  7 -> 9 [label="ll: noop"]
  7 -> 9 [label="lr: noop"]
  7 -> 4 [label="ul: out (char)"]
  7 -> 5 [label="ur: roll"]
  9 -> 17 [label="dl: push 16"]
  9 -> 17 [label="dr: push 16"]
  9 -> 12 [label="ll: switch"]
  9 -> 12 [label="lr: switch"]
  9 -> 1 [label="ul: duplicate"]
  9 -> 1 [label="ur: duplicate"]
  12 -> 9 [label="rl: pointer"]
  12 -> 9 [label="rr: pointer"]
  12 -> 23 [label="dl: noop"]
  12 -> 6 [label="ul: add"]
  12 -> 6 [label="ur: add"]
  15 -> 18 [label="dl: duplicate"]
  15 -> 18 [label="dr: duplicate"]
  15 -> 9 [label="ll: roll"]
  15 -> 9 [label="lr: roll"]
  15 -> 9 [label="ul: roll"]
  15 -> 9 [label="ur: roll"]
  17 -> 18 [label="rl: push 1"]
  17 -> 18 [label="rr: push 1"]
  17 -> 9 [label="ul: pop"]
  17 -> 9 [label="ur: pop"]
  18 -> 25 [label="dl: divide"]
  18 -> 25 [label="dr: divide"]
  18 -> 17 [label="ll: pop"]
  18 -> 17 [label="lr: pop"]
  18 -> 15 [label="ul: divide"]
  22 -> 23 [label="rl: noop"]
  22 -> 23 [label="rr: noop"]
  22 -> 12 [label="ul: noop"]
  22 -> 12 [label="ur: noop"]
  23 -> 25 [label="rl: noop"]
  23 -> 25 [label="rr: noop"]
  23 -> 22 [label="ll: noop"]
  23 -> 22 [label="lr: noop"]
  23 -> 12 [label="ul: noop"]
  23 -> 12 [label="ur: noop"]
  25 -> 23 [label="ll: noop"]
  25 -> 23 [label="lr: noop"]
  25 -> 18 [label="ul: duplicate"]
  25 -> 18 [label="ur: duplicate"]
}|]
