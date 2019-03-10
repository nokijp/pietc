{-# LANGUAGE QuasiQuotes #-}

module Language.Piet.AssemblyGeneratorSpec
  ( main
  , spec
  ) where

import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Language.Piet.AssemblyGenerator
import Language.Piet.Syntax
import LLVM.Pretty
import Test.Hspec
import Text.InterpolatedString.Perl6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "generateAssembly" $ do
    forM_
      [ ("emptyGraph", SyntaxGraph IM.empty, emptyIR)
      , ("smallestGraph", smallestGraph, smallestIR)
      , ("complexGraph", complexGraph, complexIR)
      ] $ \(name, inputGraph, expected) ->
        context ("when given " ++ name) $ do
          let ast = generateAssembly inputGraph
          it "generates a correct LLVM AST" $ normalize (ppllvm ast) `shouldBe` normalize expected

normalize :: Text -> Text
normalize = T.unlines . filter (not . T.null) . fmap (T.unwords . T.words) . T.lines

emptyIR :: Text
emptyIR = [q|
; ModuleID = ''

declare external ccc void @push(i32)
declare external ccc void @pointer(i32*)
declare external ccc void @switch(i32*)
declare external ccc void @pop()
declare external ccc void @add()
declare external ccc void @subtract()
declare external ccc void @multiply()
declare external ccc void @divide()
declare external ccc void @mod()
declare external ccc void @not()
declare external ccc void @greater()
declare external ccc void @duplicate()
declare external ccc void @roll()
declare external ccc void @in_number()
declare external ccc void @in_char()
declare external ccc void @out_number()
declare external ccc void @out_char()
declare external ccc void @reset_stack()

define external ccc i32 @main() {
entry:
  %dpcc_ptr = alloca i32, align 4
  store i32 0, i32* %dpcc_ptr, align 4
  br label %exit

exit:
  call ccc void @reset_stack()
  ret i32 0
}
|]

smallestGraph :: SyntaxGraph
smallestGraph = SyntaxGraph $ IM.singleton 0 (Block M.empty)

smallestIR :: Text
smallestIR = [q|
; ModuleID = ''

declare external ccc void @push(i32)
declare external ccc void @pointer(i32*)
declare external ccc void @switch(i32*)
declare external ccc void @pop()
declare external ccc void @add()
declare external ccc void @subtract()
declare external ccc void @multiply()
declare external ccc void @divide()
declare external ccc void @mod()
declare external ccc void @not()
declare external ccc void @greater()
declare external ccc void @duplicate()
declare external ccc void @roll()
declare external ccc void @in_number()
declare external ccc void @in_char()
declare external ccc void @out_number()
declare external ccc void @out_char()
declare external ccc void @reset_stack()

define external ccc i32 @main() {
entry:
  %dpcc_ptr = alloca i32, align 4
  store i32 0, i32* %dpcc_ptr, align 4
  br label %block_0

block_0:
  %dpcc_0 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_0, label %exit []

exit:
  call ccc void @reset_stack()
  ret i32 0
}
|]

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

complexIR :: Text
complexIR = [q|
; ModuleID = ''

declare external ccc void @push(i32)
declare external ccc void @pointer(i32*)
declare external ccc void @switch(i32*)
declare external ccc void @pop()
declare external ccc void @add()
declare external ccc void @subtract()
declare external ccc void @multiply()
declare external ccc void @divide()
declare external ccc void @mod()
declare external ccc void @not()
declare external ccc void @greater()
declare external ccc void @duplicate()
declare external ccc void @roll()
declare external ccc void @in_number()
declare external ccc void @in_char()
declare external ccc void @out_number()
declare external ccc void @out_char()
declare external ccc void @reset_stack()

define external ccc i32 @main() {
entry:
  %dpcc_ptr = alloca i32, align 4
  store i32 0, i32* %dpcc_ptr, align 4
  br label %block_0

block_0:
  %dpcc_0 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_0, label %exit [i32 0, label %jump_0_1_rl i32 1, label %jump_0_1_rr i32 2, label %jump_0_1_dl i32 3, label %jump_0_6_dr i32 4, label %jump_0_1_rl i32 5, label %jump_0_1_rr i32 6, label %jump_0_1_rr i32 7, label %jump_0_1_rl]
jump_0_1_rl:
  store i32 0, i32* %dpcc_ptr, align 4
  call ccc void @pop()
  br label %block_1
jump_0_1_rr:
  store i32 1, i32* %dpcc_ptr, align 4
  call ccc void @pop()
  br label %block_1
jump_0_1_dl:
  call ccc void @pop()
  br label %block_1
jump_0_6_dr:
  call ccc void @push(i32 5)
  br label %block_6

block_1:
  %dpcc_1 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_1, label %exit [i32 0, label %jump_1_7_rl i32 1, label %jump_1_7_rr i32 2, label %jump_1_9_dl i32 3, label %jump_1_9_dr i32 4, label %jump_1_6_ll i32 5, label %jump_1_6_lr i32 6, label %jump_1_7_rr i32 7, label %jump_1_7_rl]
jump_1_7_rl:
  store i32 0, i32* %dpcc_ptr, align 4
  br label %block_7
jump_1_7_rr:
  store i32 1, i32* %dpcc_ptr, align 4
  br label %block_7
jump_1_9_dl:
  call ccc void @divide()
  br label %block_9
jump_1_9_dr:
  call ccc void @divide()
  br label %block_9
jump_1_6_ll:
  call ccc void @pop()
  br label %block_6
jump_1_6_lr:
  call ccc void @pop()
  br label %block_6

block_2:
  %dpcc_2 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_2, label %exit [i32 0, label %jump_2_4_rl i32 1, label %jump_2_4_rr i32 2, label %jump_2_9_dl i32 3, label %jump_2_1_dr i32 4, label %jump_2_1_ll i32 5, label %jump_2_1_lr i32 6, label %jump_2_4_rr i32 7, label %jump_2_4_rl]
jump_2_4_rl:
  store i32 0, i32* %dpcc_ptr, align 4
  br label %block_4
jump_2_4_rr:
  store i32 1, i32* %dpcc_ptr, align 4
  br label %block_4
jump_2_9_dl:
  br label %block_9
jump_2_1_dr:
  call ccc void @roll()
  br label %block_1
jump_2_1_ll:
  call ccc void @roll()
  br label %block_1
jump_2_1_lr:
  call ccc void @roll()
  br label %block_1

block_4:
  %dpcc_4 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_4, label %exit [i32 0, label %jump_4_5_rl i32 1, label %jump_4_5_rr i32 2, label %jump_4_7_dl i32 3, label %jump_4_7_dr i32 4, label %jump_4_2_ll i32 5, label %jump_4_2_lr i32 6, label %jump_4_5_rr i32 7, label %jump_4_5_rl]
jump_4_5_rl:
  store i32 0, i32* %dpcc_ptr, align 4
  call ccc void @out_char()
  br label %block_5
jump_4_5_rr:
  store i32 1, i32* %dpcc_ptr, align 4
  call ccc void @out_char()
  br label %block_5
jump_4_7_dl:
  call ccc void @subtract()
  br label %block_7
jump_4_7_dr:
  br label %block_7
jump_4_2_ll:
  br label %block_2
jump_4_2_lr:
  br label %block_2

block_5:
  %dpcc_5 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_5, label %exit [i32 0, label %jump_5_7_dr i32 1, label %jump_5_7_dr i32 2, label %jump_5_7_dr i32 3, label %jump_5_7_dr i32 4, label %jump_5_4_ll i32 5, label %jump_5_4_lr i32 6, label %jump_5_7_dr i32 7, label %jump_5_7_dr]
jump_5_7_dr:
  store i32 3, i32* %dpcc_ptr, align 4
  call ccc void @not()
  br label %block_7
jump_5_4_ll:
  call ccc void @subtract()
  br label %block_4
jump_5_4_lr:
  call ccc void @subtract()
  br label %block_4

block_6:
  %dpcc_6 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_6, label %exit [i32 0, label %jump_6_9_rl i32 1, label %jump_6_9_rr i32 2, label %jump_6_9_dl i32 3, label %jump_6_12_dr i32 4, label %jump_6_0_ur i32 5, label %jump_6_0_ul i32 6, label %jump_6_0_ul i32 7, label %jump_6_0_ur]
jump_6_9_rl:
  call ccc void @mod()
  br label %block_9
jump_6_9_rr:
  call ccc void @mod()
  br label %block_9
jump_6_9_dl:
  call ccc void @mod()
  br label %block_9
jump_6_12_dr:
  call ccc void @in_char()
  br label %block_12
jump_6_0_ul:
  store i32 6, i32* %dpcc_ptr, align 4
  call ccc void @pop()
  br label %block_0
jump_6_0_ur:
  store i32 7, i32* %dpcc_ptr, align 4
  call ccc void @pop()
  br label %block_0

block_7:
  %dpcc_7 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_7, label %exit [i32 0, label %jump_7_9_ll i32 1, label %jump_7_9_lr i32 2, label %jump_7_9_lr i32 3, label %jump_7_9_ll i32 4, label %jump_7_9_ll i32 5, label %jump_7_9_lr i32 6, label %jump_7_4_ul i32 7, label %jump_7_5_ur]
jump_7_9_ll:
  store i32 4, i32* %dpcc_ptr, align 4
  br label %block_9
jump_7_9_lr:
  store i32 5, i32* %dpcc_ptr, align 4
  br label %block_9
jump_7_4_ul:
  call ccc void @out_char()
  br label %block_4
jump_7_5_ur:
  call ccc void @roll()
  br label %block_5

block_9:
  %dpcc_9 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_9, label %exit [i32 0, label %jump_9_17_dr i32 1, label %jump_9_17_dl i32 2, label %jump_9_17_dl i32 3, label %jump_9_17_dr i32 4, label %jump_9_12_ll i32 5, label %jump_9_12_lr i32 6, label %jump_9_1_ul i32 7, label %jump_9_1_ur]
jump_9_17_dl:
  store i32 2, i32* %dpcc_ptr, align 4
  call ccc void @push(i32 16)
  br label %block_17
jump_9_17_dr:
  store i32 3, i32* %dpcc_ptr, align 4
  call ccc void @push(i32 16)
  br label %block_17
jump_9_12_ll:
  call ccc void @switch(i32* %dpcc_ptr)
  br label %block_12
jump_9_12_lr:
  call ccc void @switch(i32* %dpcc_ptr)
  br label %block_12
jump_9_1_ul:
  call ccc void @duplicate()
  br label %block_1
jump_9_1_ur:
  call ccc void @duplicate()
  br label %block_1

block_12:
  %dpcc_12 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_12, label %exit [i32 0, label %jump_12_9_rl i32 1, label %jump_12_9_rr i32 2, label %jump_12_23_dl i32 3, label %jump_12_23_dl i32 4, label %jump_12_6_ur i32 5, label %jump_12_6_ul i32 6, label %jump_12_6_ul i32 7, label %jump_12_6_ur]
jump_12_9_rl:
  call ccc void @pointer(i32* %dpcc_ptr)
  br label %block_9
jump_12_9_rr:
  call ccc void @pointer(i32* %dpcc_ptr)
  br label %block_9
jump_12_23_dl:
  store i32 2, i32* %dpcc_ptr, align 4
  br label %block_23
jump_12_6_ul:
  store i32 6, i32* %dpcc_ptr, align 4
  call ccc void @add()
  br label %block_6
jump_12_6_ur:
  store i32 7, i32* %dpcc_ptr, align 4
  call ccc void @add()
  br label %block_6

block_15:
  %dpcc_15 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_15, label %exit [i32 0, label %jump_15_18_dr i32 1, label %jump_15_18_dl i32 2, label %jump_15_18_dl i32 3, label %jump_15_18_dr i32 4, label %jump_15_9_ll i32 5, label %jump_15_9_lr i32 6, label %jump_15_9_ul i32 7, label %jump_15_9_ur]
jump_15_18_dl:
  store i32 2, i32* %dpcc_ptr, align 4
  call ccc void @duplicate()
  br label %block_18
jump_15_18_dr:
  store i32 3, i32* %dpcc_ptr, align 4
  call ccc void @duplicate()
  br label %block_18
jump_15_9_ll:
  call ccc void @roll()
  br label %block_9
jump_15_9_lr:
  call ccc void @roll()
  br label %block_9
jump_15_9_ul:
  call ccc void @roll()
  br label %block_9
jump_15_9_ur:
  call ccc void @roll()
  br label %block_9

block_17:
  %dpcc_17 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_17, label %exit [i32 0, label %jump_17_18_rl i32 1, label %jump_17_18_rr i32 2, label %jump_17_9_ul i32 3, label %jump_17_9_ur i32 4, label %jump_17_9_ur i32 5, label %jump_17_9_ul i32 6, label %jump_17_9_ul i32 7, label %jump_17_9_ur]
jump_17_18_rl:
  call ccc void @push(i32 1)
  br label %block_18
jump_17_18_rr:
  call ccc void @push(i32 1)
  br label %block_18
jump_17_9_ul:
  store i32 6, i32* %dpcc_ptr, align 4
  call ccc void @pop()
  br label %block_9
jump_17_9_ur:
  store i32 7, i32* %dpcc_ptr, align 4
  call ccc void @pop()
  br label %block_9

block_18:
  %dpcc_18 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_18, label %exit [i32 0, label %jump_18_25_dr i32 1, label %jump_18_25_dl i32 2, label %jump_18_25_dl i32 3, label %jump_18_25_dr i32 4, label %jump_18_17_ll i32 5, label %jump_18_17_lr i32 6, label %jump_18_15_ul i32 7, label %jump_18_15_ul]
jump_18_25_dl:
  store i32 2, i32* %dpcc_ptr, align 4
  call ccc void @divide()
  br label %block_25
jump_18_25_dr:
  store i32 3, i32* %dpcc_ptr, align 4
  call ccc void @divide()
  br label %block_25
jump_18_17_ll:
  call ccc void @pop()
  br label %block_17
jump_18_17_lr:
  call ccc void @pop()
  br label %block_17
jump_18_15_ul:
  store i32 6, i32* %dpcc_ptr, align 4
  call ccc void @divide()
  br label %block_15

block_22:
  %dpcc_22 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_22, label %exit [i32 0, label %jump_22_23_rl i32 1, label %jump_22_23_rr i32 2, label %jump_22_12_ul i32 3, label %jump_22_12_ur i32 4, label %jump_22_12_ur i32 5, label %jump_22_12_ul i32 6, label %jump_22_12_ul i32 7, label %jump_22_12_ur]
jump_22_23_rl:
  br label %block_23
jump_22_23_rr:
  br label %block_23
jump_22_12_ul:
  store i32 6, i32* %dpcc_ptr, align 4
  br label %block_12
jump_22_12_ur:
  store i32 7, i32* %dpcc_ptr, align 4
  br label %block_12

block_23:
  %dpcc_23 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_23, label %exit [i32 0, label %jump_23_25_rl i32 1, label %jump_23_25_rr i32 2, label %jump_23_22_lr i32 3, label %jump_23_22_ll i32 4, label %jump_23_22_ll i32 5, label %jump_23_22_lr i32 6, label %jump_23_12_ul i32 7, label %jump_23_12_ur]
jump_23_25_rl:
  br label %block_25
jump_23_25_rr:
  br label %block_25
jump_23_22_ll:
  store i32 4, i32* %dpcc_ptr, align 4
  br label %block_22
jump_23_22_lr:
  store i32 5, i32* %dpcc_ptr, align 4
  br label %block_22
jump_23_12_ul:
  br label %block_12
jump_23_12_ur:
  br label %block_12

block_25:
  %dpcc_25 = load i32, i32* %dpcc_ptr, align 4
  switch i32 %dpcc_25, label %exit [i32 0, label %jump_25_23_ll i32 1, label %jump_25_23_lr i32 2, label %jump_25_23_lr i32 3, label %jump_25_23_ll i32 4, label %jump_25_23_ll i32 5, label %jump_25_23_lr i32 6, label %jump_25_18_ul i32 7, label %jump_25_18_ur]
jump_25_23_ll:
  store i32 4, i32* %dpcc_ptr, align 4
  br label %block_23
jump_25_23_lr:
  store i32 5, i32* %dpcc_ptr, align 4
  br label %block_23
jump_25_18_ul:
  call ccc void @duplicate()
  br label %block_18
jump_25_18_ur:
  call ccc void @duplicate()
  br label %block_18

exit:
  call ccc void @reset_stack()
  ret i32 0
}
|]
