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
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (Pop, 1))
                                                                , ((DPRight, CCRight), (Pop, 1))
                                                                , ((DPDown, CCLeft), (Pop, 1))
                                                                , ((DPDown, CCRight), (Push 5, 6))
                                                                ]
                                           )
                                         , ( 1
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (NoOperation, 7))
                                                                , ((DPRight, CCRight), (NoOperation, 7))
                                                                , ((DPDown, CCLeft), (Divide, 9))
                                                                , ((DPDown, CCRight), (Divide, 9))
                                                                , ((DPLeft, CCLeft), (Pop, 6))
                                                                , ((DPLeft, CCRight), (Pop, 6))
                                                                ]
                                           )
                                         , ( 2
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (NoOperation, 4))
                                                                , ((DPRight, CCRight), (NoOperation, 4))
                                                                , ((DPDown, CCLeft), (NoOperation, 9))
                                                                , ((DPDown, CCRight), (Roll, 1))
                                                                , ((DPLeft, CCLeft), (Roll, 1))
                                                                , ((DPLeft, CCRight), (Roll, 1))
                                                                ]
                                           )
                                         , ( 4
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (OutChar, 5))
                                                                , ((DPRight, CCRight), (OutChar, 5))
                                                                , ((DPDown, CCLeft), (Subtract, 7))
                                                                , ((DPDown, CCRight), (NoOperation, 7))
                                                                , ((DPLeft, CCLeft), (NoOperation, 2))
                                                                , ((DPLeft, CCRight), (NoOperation, 2))
                                                                ]
                                           )
                                         , ( 5
                                           , Block $ M.fromList [ ((DPDown, CCRight), (Not, 7))
                                                                , ((DPLeft, CCLeft), (Subtract, 4))
                                                                , ((DPLeft, CCRight), (Subtract, 4))
                                                                ]
                                           )
                                         , ( 6
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (Mod, 9))
                                                                , ((DPRight, CCRight), (Mod, 9))
                                                                , ((DPDown, CCLeft), (Mod, 9))
                                                                , ((DPDown, CCRight), (InChar, 12))
                                                                , ((DPUp, CCLeft), (Pop, 0))
                                                                , ((DPUp, CCRight), (Pop, 0))
                                                                ]
                                           )
                                         , ( 7
                                           , Block $ M.fromList [ ((DPLeft, CCLeft), (NoOperation, 9))
                                                                , ((DPLeft, CCRight), (NoOperation, 9))
                                                                , ((DPUp, CCLeft), (OutChar, 4))
                                                                , ((DPUp, CCRight), (Roll, 5))
                                                                ]
                                           )
                                         , ( 9
                                           , Block $ M.fromList [ ((DPDown, CCLeft), (Push 16, 17))
                                                                , ((DPDown, CCRight), (Push 16, 17))
                                                                , ((DPLeft, CCLeft), (Switch, 12))
                                                                , ((DPLeft, CCRight), (Switch, 12))
                                                                , ((DPUp, CCLeft), (Duplicate, 1))
                                                                , ((DPUp, CCRight), (Duplicate, 1))
                                                                ]
                                           )
                                         , ( 12
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (Pointer, 9))
                                                                , ((DPRight, CCRight), (Pointer, 9))
                                                                , ((DPDown, CCLeft), (NoOperation, 23))
                                                                , ((DPUp, CCLeft), (Add, 6))
                                                                , ((DPUp, CCRight), (Add, 6))
                                                                ]
                                           )
                                         , ( 15
                                           , Block $ M.fromList [ ((DPDown, CCLeft), (Duplicate, 18))
                                                                , ((DPDown, CCRight), (Duplicate, 18))
                                                                , ((DPLeft, CCLeft), (Roll, 9))
                                                                , ((DPLeft, CCRight), (Roll, 9))
                                                                , ((DPUp, CCLeft), (Roll, 9))
                                                                , ((DPUp, CCRight), (Roll, 9))
                                                                ]
                                           )
                                         , ( 17
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (Push 1, 18))
                                                                , ((DPRight, CCRight), (Push 1, 18))
                                                                , ((DPUp, CCLeft), (Pop, 9))
                                                                , ((DPUp, CCRight), (Pop, 9))
                                                                ]
                                           )
                                         , ( 18
                                           , Block $ M.fromList [ ((DPDown, CCLeft), (Divide, 25))
                                                                , ((DPDown, CCRight), (Divide, 25))
                                                                , ((DPLeft, CCLeft), (Pop, 17))
                                                                , ((DPLeft, CCRight), (Pop, 17))
                                                                , ((DPUp, CCLeft), (Divide, 15))
                                                                ]
                                           )
                                         , ( 22
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (NoOperation, 23))
                                                                , ((DPRight, CCRight), (NoOperation, 23))
                                                                , ((DPUp, CCLeft), (NoOperation, 12))
                                                                , ((DPUp, CCRight), (NoOperation, 12))
                                                                ]
                                           )
                                         , ( 23
                                           , Block $ M.fromList [ ((DPRight, CCLeft), (NoOperation, 25))
                                                                , ((DPRight, CCRight), (NoOperation, 25))
                                                                , ((DPLeft, CCLeft), (NoOperation, 22))
                                                                , ((DPLeft, CCRight), (NoOperation, 22))
                                                                , ((DPUp, CCLeft), (NoOperation, 12))
                                                                , ((DPUp, CCRight), (NoOperation, 12))
                                                                ]
                                           )
                                         , ( 25
                                           , Block $ M.fromList [ ((DPLeft, CCLeft), (NoOperation, 23))
                                                                , ((DPLeft, CCRight), (NoOperation, 23))
                                                                , ((DPUp, CCLeft), (Duplicate, 18))
                                                                , ((DPUp, CCRight), (Duplicate, 18))
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
