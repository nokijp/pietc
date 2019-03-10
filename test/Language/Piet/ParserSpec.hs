module Language.Piet.ParserSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import Language.Piet.Codel
import Language.Piet.Parser
import Language.Piet.Syntax
import Test.Hspec
import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "returns a syntax graph when given an image" $ parse rawComplexImage `shouldBe` Right expectedComplexGraph

  describe "parseFilledImage" $ do
    forM_
      [ ("when given a small image", smallImage, smallBlockTable, expectedSmallGraph)
      , ("when given a complex image", complexImage, complexBlockTable, expectedComplexGraph)
      ] $ \(name, image, blockTable, expectedGraph) ->
        context name $ do
          it "returns a syntax graph" $ parseFilledImage (image, blockTable) `shouldBe` Right expectedGraph

    forM_
      [ ("when given an empty image", V.empty, IM.empty, EmptyBlockTableError)
      , ("when given a white image", whiteImage, whiteBlockTable, IllegalInitialColorError WhiteCodel)
      , ("when given a black image", blackImage, blackBlockTable, IllegalInitialColorError BlackCodel)
      ] $ \(name, image, blockTable, expectedError) ->
        context name $ do
          it "returns an error" $ parseFilledImage (image, blockTable) `shouldBe` Left expectedError

    context "when given an image which only consists of two pixels" $ do
      forM_
        [ (AchromaticCodel Red Light, AchromaticCodel Red Normal, Push 1, Pop)
        , (AchromaticCodel Red Light, AchromaticCodel Red Dark, Pop, Push 1)
        , (AchromaticCodel Red Light, AchromaticCodel Yellow Light, Add, InChar)
        , (AchromaticCodel Red Light, AchromaticCodel Yellow Normal, Subtract, OutChar)
        , (AchromaticCodel Red Light, AchromaticCodel Yellow Dark, Multiply, OutNumber)
        , (AchromaticCodel Red Light, AchromaticCodel Green Light, Divide, Duplicate)
        , (AchromaticCodel Red Light, AchromaticCodel Green Normal, Mod, InNumber)
        , (AchromaticCodel Red Light, AchromaticCodel Green Dark, Not, Roll)
        , (AchromaticCodel Red Light, AchromaticCodel Cyan Light, Greater, Greater)
        , (AchromaticCodel Red Light, AchromaticCodel Cyan Normal, Pointer, Switch)
        , (AchromaticCodel Red Light, AchromaticCodel Cyan Dark, Switch, Pointer)
        , (AchromaticCodel Red Light, AchromaticCodel Blue Light, Duplicate, Divide)
        , (AchromaticCodel Red Light, AchromaticCodel Blue Normal, Roll, Not)
        , (AchromaticCodel Red Light, AchromaticCodel Blue Dark, InNumber, Mod)
        , (AchromaticCodel Red Light, AchromaticCodel Magenta Light, InChar, Add)
        , (AchromaticCodel Red Light, AchromaticCodel Magenta Normal, OutNumber, Multiply)
        , (AchromaticCodel Red Light, AchromaticCodel Magenta Dark, OutChar, Subtract)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Cyan Light, Push 1, Pop)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Cyan Normal, Pop, Push 1)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, Add, InChar)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Blue Light, Subtract, OutChar)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Blue Normal, Multiply, OutNumber)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Magenta Dark, Divide, Duplicate)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Magenta Light, Mod, InNumber)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Magenta Normal, Not, Roll)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Red Dark, Greater, Greater)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Red Light, Pointer, Switch)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Red Normal, Switch, Pointer)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Yellow Dark, Duplicate, Divide)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Yellow Light, Roll, Not)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Yellow Normal, InNumber, Mod)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Green Dark, InChar, Add)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Green Light, OutNumber, Multiply)
        , (AchromaticCodel Cyan Dark, AchromaticCodel Green Normal, OutChar, Subtract)
        ] $ \(color1, color2, command12, command21) -> do
          let image = toVector2D [[(color1, 0), (color2, 1)]]
          let blockTable = IM.fromList [(0, [(0, 0)]), (1, [(1, 0)])]
          let expectedGraph = SyntaxGraph $ IM.fromList [ ( 0
                                                          , Block $ M.fromList [ (DPCC DPRight CCLeft, (command12, 1))
                                                                               , (DPCC DPRight CCRight, (command12, 1))
                                                                               ]
                                                          )
                                                        , ( 1
                                                          , Block $ M.fromList [ (DPCC DPLeft CCLeft, (command21, 0))
                                                                               , (DPCC DPLeft CCRight, (command21, 0))
                                                                               ]
                                                          )
                                                        ]
          it ("returns " ++ show (command12, command21) ++ " when given " ++ show (color1, color2)) $ parseFilledImage (image, blockTable) `shouldBe` Right expectedGraph

smallImage :: Vector (Vector (Codel, Int))
smallImage = toVector2D [[(AchromaticCodel Red Normal, 0)]]

smallBlockTable :: IntMap [(Int, Int)]
smallBlockTable = IM.fromList [(0, [(0, 0)])]

expectedSmallGraph :: SyntaxGraph
expectedSmallGraph = SyntaxGraph $ IM.fromList [(0, Block $ M.fromList [])]

whiteImage :: Vector (Vector (Codel, Int))
whiteImage = toVector2D [[(WhiteCodel, 0)]]

whiteBlockTable :: IntMap [(Int, Int)]
whiteBlockTable = IM.fromList [(0, [(0, 0)])]

blackImage :: Vector (Vector (Codel, Int))
blackImage = toVector2D [[(BlackCodel, 0)]]

blackBlockTable :: IntMap [(Int, Int)]
blackBlockTable = IM.fromList [(0, [(0, 0)])]

rawComplexImage :: Vector (Vector Codel)
rawComplexImage = toVector2D [ [ AchromaticCodel Blue Dark
                               , AchromaticCodel Blue Dark
                               , AchromaticCodel Blue Dark
                               , AchromaticCodel Blue Dark
                               , AchromaticCodel Blue Dark
                               , AchromaticCodel Blue Normal
                               , AchromaticCodel Red Light
                               , AchromaticCodel Red Light
                               , AchromaticCodel Red Light
                               , WhiteCodel
                               , AchromaticCodel Red Light
                               , AchromaticCodel Red Light
                               , AchromaticCodel Red Light
                               , AchromaticCodel Magenta Dark
                               , AchromaticCodel Magenta Dark
                               , AchromaticCodel Magenta Dark
                               ]
                             , [ AchromaticCodel Blue Light
                               , AchromaticCodel Blue Light
                               , AchromaticCodel Blue Light
                               , AchromaticCodel Blue Normal
                               , AchromaticCodel Blue Normal
                               , AchromaticCodel Blue Normal
                               , AchromaticCodel Blue Normal
                               , AchromaticCodel Blue Normal
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , AchromaticCodel Yellow Normal
                               , AchromaticCodel Yellow Normal
                               , AchromaticCodel Yellow Normal
                               , BlackCodel
                               ]
                             , [ AchromaticCodel Blue Light
                               , AchromaticCodel Blue Light
                               , AchromaticCodel Blue Light
                               , AchromaticCodel Blue Light
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Blue Normal
                               , AchromaticCodel Blue Normal
                               , AchromaticCodel Red Normal
                               , WhiteCodel
                               , WhiteCodel
                               , AchromaticCodel Yellow Normal
                               , AchromaticCodel Yellow Normal
                               , AchromaticCodel Yellow Normal
                               , BlackCodel
                               , BlackCodel
                               , AchromaticCodel Magenta Light
                               ]
                             , [ AchromaticCodel Cyan Light
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , BlackCodel
                               , BlackCodel
                               , BlackCodel
                               , BlackCodel
                               , BlackCodel
                               , AchromaticCodel Magenta Light
                               , AchromaticCodel Magenta Light
                               ]
                             , [ WhiteCodel
                               , WhiteCodel
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Red Normal
                               , BlackCodel
                               , AchromaticCodel Magenta Light
                               , AchromaticCodel Magenta Light
                               , BlackCodel
                               ]
                             , [ WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Cyan Light
                               , AchromaticCodel Red Normal
                               , AchromaticCodel Green Light
                               , BlackCodel
                               , BlackCodel
                               , AchromaticCodel Magenta Light
                               , AchromaticCodel Magenta Light
                               , AchromaticCodel Magenta Light
                               , BlackCodel
                               ]
                             , [ WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , AchromaticCodel Red Dark
                               , AchromaticCodel Red Light
                               , AchromaticCodel Red Light
                               , AchromaticCodel Red Light
                               , BlackCodel
                               , AchromaticCodel Green Dark
                               , AchromaticCodel Green Dark
                               , AchromaticCodel Red Light
                               ]
                             , [ WhiteCodel
                               , AchromaticCodel Yellow Light
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , AchromaticCodel Cyan Dark
                               , AchromaticCodel Cyan Dark
                               , WhiteCodel
                               , AchromaticCodel Green Light
                               , AchromaticCodel Green Light
                               , AchromaticCodel Green Light
                               , WhiteCodel
                               , WhiteCodel
                               , WhiteCodel
                               , BlackCodel
                               ]
                             ]

complexImage :: Vector (Vector (Codel, Int))
complexImage = toVector2D [ [ (AchromaticCodel Blue Dark, 0)
                            , (AchromaticCodel Blue Dark, 0)
                            , (AchromaticCodel Blue Dark, 0)
                            , (AchromaticCodel Blue Dark, 0)
                            , (AchromaticCodel Blue Dark, 0)
                            , (AchromaticCodel Blue Normal, 1)
                            , (AchromaticCodel Red Light, 2)
                            , (AchromaticCodel Red Light, 2)
                            , (AchromaticCodel Red Light, 2)
                            , (WhiteCodel, 3)
                            , (AchromaticCodel Red Light, 4)
                            , (AchromaticCodel Red Light, 4)
                            , (AchromaticCodel Red Light, 4)
                            , (AchromaticCodel Magenta Dark, 5)
                            , (AchromaticCodel Magenta Dark, 5)
                            , (AchromaticCodel Magenta Dark, 5)
                            ]
                          , [ (AchromaticCodel Blue Light, 6)
                            , (AchromaticCodel Blue Light, 6)
                            , (AchromaticCodel Blue Light, 6)
                            , (AchromaticCodel Blue Normal, 1)
                            , (AchromaticCodel Blue Normal, 1)
                            , (AchromaticCodel Blue Normal, 1)
                            , (AchromaticCodel Blue Normal, 1)
                            , (AchromaticCodel Blue Normal, 1)
                            , (WhiteCodel, 3)
                            , (WhiteCodel, 3)
                            , (WhiteCodel, 3)
                            , (WhiteCodel, 3)
                            , (AchromaticCodel Yellow Normal, 7)
                            , (AchromaticCodel Yellow Normal, 7)
                            , (AchromaticCodel Yellow Normal, 7)
                            , (BlackCodel, 8)
                            ]
                          , [ (AchromaticCodel Blue Light, 6)
                            , (AchromaticCodel Blue Light, 6)
                            , (AchromaticCodel Blue Light, 6)
                            , (AchromaticCodel Blue Light, 6)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Blue Normal, 1)
                            , (AchromaticCodel Blue Normal, 1)
                            , (AchromaticCodel Red Normal, 9)
                            , (WhiteCodel, 3)
                            , (WhiteCodel, 3)
                            , (AchromaticCodel Yellow Normal, 7)
                            , (AchromaticCodel Yellow Normal, 7)
                            , (AchromaticCodel Yellow Normal, 7)
                            , (BlackCodel, 10)
                            , (BlackCodel, 10)
                            , (AchromaticCodel Magenta Light, 11)
                            ]
                          , [ (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (BlackCodel, 10)
                            , (BlackCodel, 10)
                            , (BlackCodel, 10)
                            , (BlackCodel, 10)
                            , (BlackCodel, 10)
                            , (AchromaticCodel Magenta Light, 11)
                            , (AchromaticCodel Magenta Light, 11)
                            ]
                          , [ (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Red Normal, 9)
                            , (BlackCodel, 10)
                            , (AchromaticCodel Magenta Light, 11)
                            , (AchromaticCodel Magenta Light, 11)
                            , (BlackCodel, 14)
                            ]
                          , [ (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Cyan Light, 12)
                            , (AchromaticCodel Red Normal, 9)
                            , (AchromaticCodel Green Light, 15)
                            , (BlackCodel, 16)
                            , (BlackCodel, 16)
                            , (AchromaticCodel Magenta Light, 11)
                            , (AchromaticCodel Magenta Light, 11)
                            , (AchromaticCodel Magenta Light, 11)
                            , (BlackCodel, 14)
                            ]
                          , [ (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (AchromaticCodel Red Dark, 17)
                            , (AchromaticCodel Red Light, 18)
                            , (AchromaticCodel Red Light, 18)
                            , (AchromaticCodel Red Light, 18)
                            , (BlackCodel, 19)
                            , (AchromaticCodel Green Dark, 20)
                            , (AchromaticCodel Green Dark, 20)
                            , (AchromaticCodel Red Light, 21)
                            ]
                          , [ (WhiteCodel, 13)
                            , (AchromaticCodel Yellow Light, 22)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (WhiteCodel, 13)
                            , (AchromaticCodel Cyan Dark, 23)
                            , (AchromaticCodel Cyan Dark, 23)
                            , (WhiteCodel, 24)
                            , (AchromaticCodel Green Light, 25)
                            , (AchromaticCodel Green Light, 25)
                            , (AchromaticCodel Green Light, 25)
                            , (WhiteCodel, 26)
                            , (WhiteCodel, 26)
                            , (WhiteCodel, 26)
                            , (BlackCodel, 27)
                            ]
                          ]

complexBlockTable :: IntMap [(Int, Int)]
complexBlockTable = IM.fromList [ (0, [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)])
                                , (1, [(5, 0), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1), (5, 2), (6, 2)])
                                , (2, [(6, 0), (7, 0), (8, 0)])
                                , (3, [(9, 0), (8, 1), (9, 1), (10, 1), (11, 1), (8, 2), (9, 2)])
                                , (4, [(10, 0), (11, 0), (12, 0)])
                                , (5, [(13, 0), (14, 0), (15, 0)])
                                , (6, [(0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2), (3, 2)])
                                , (7, [(12, 1), (13, 1), (14, 1), (10, 2), (11, 2), (12, 2)])
                                , (8, [(15, 1)])
                                , (9, [(4, 2), (7, 2), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (5, 4), (6, 4), (7, 4), (8, 4), (9, 4), (10, 4), (11, 4), (8, 5)])
                                , (10, [(13, 2), (14, 2), (9, 3), (10, 3), (11, 3), (12, 3), (13, 3), (12, 4)])
                                , (11, [(15, 2), (14, 3), (15, 3), (13, 4), (14, 4), (12, 5), (13, 5), (14, 5)])
                                , (12, [(0, 3), (1, 3), (2, 3), (2, 4), (3, 4), (4, 4), (3, 5), (4, 5), (5, 5), (6, 5), (7, 5)])
                                , (13, [(0, 4), (1, 4), (0, 5), (1, 5), (2, 5), (0, 6), (1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (7, 6), (0, 7), (2, 7), (3, 7), (4, 7), (5, 7)])
                                , (14, [(15, 4), (15, 5)])
                                , (15, [(9, 5)])
                                , (16, [(10, 5), (11, 5)])
                                , (17, [(8, 6)])
                                , (18, [(9, 6), (10, 6), (11, 6)])
                                , (19, [(12, 6)])
                                , (20, [(13, 6), (14, 6)])
                                , (21, [(15, 6)])
                                , (22, [(1, 7)])
                                , (23, [(6, 7), (7, 7)])
                                , (24, [(8, 7)])
                                , (25, [(9, 7), (10, 7), (11, 7)])
                                , (26, [(12, 7), (13, 7), (14, 7)])
                                , (27, [(15, 7)])
                                ]

expectedComplexGraph :: SyntaxGraph
expectedComplexGraph = SyntaxGraph $ IM.fromList [ ( 0
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (Pop, 1))
                                                                        , (DPCC DPRight CCRight, (Pop, 1))
                                                                        , (DPCC DPDown CCLeft, (Pop, 1))
                                                                        , (DPCC DPDown CCRight, (Push 5, 6))
                                                                        ]
                                                   )
                                                 , ( 1
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (NoOperation, 7))
                                                                        , (DPCC DPRight CCRight, (NoOperation, 7))
                                                                        , (DPCC DPDown CCLeft, (Divide, 9))
                                                                        , (DPCC DPDown CCRight, (Divide, 9))
                                                                        , (DPCC DPLeft CCLeft, (Pop, 6))
                                                                        , (DPCC DPLeft CCRight, (Pop, 6))
                                                                        ]
                                                   )
                                                 , ( 2
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (NoOperation, 4))
                                                                        , (DPCC DPRight CCRight, (NoOperation, 4))
                                                                        , (DPCC DPDown CCLeft, (NoOperation, 9))
                                                                        , (DPCC DPDown CCRight, (Roll, 1))
                                                                        , (DPCC DPLeft CCLeft, (Roll, 1))
                                                                        , (DPCC DPLeft CCRight, (Roll, 1))
                                                                        ]
                                                   )
                                                 , ( 4
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (OutChar, 5))
                                                                        , (DPCC DPRight CCRight, (OutChar, 5))
                                                                        , (DPCC DPDown CCLeft, (Subtract, 7))
                                                                        , (DPCC DPDown CCRight, (NoOperation, 7))
                                                                        , (DPCC DPLeft CCLeft, (NoOperation, 2))
                                                                        , (DPCC DPLeft CCRight, (NoOperation, 2))
                                                                        ]
                                                   )
                                                 , ( 5
                                                   , Block $ M.fromList [ (DPCC DPDown CCRight, (Not, 7))
                                                                        , (DPCC DPLeft CCLeft, (Subtract, 4))
                                                                        , (DPCC DPLeft CCRight, (Subtract, 4))
                                                                        ]
                                                   )
                                                 , ( 6
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (Mod, 9))
                                                                        , (DPCC DPRight CCRight, (Mod, 9))
                                                                        , (DPCC DPDown CCLeft, (Mod, 9))
                                                                        , (DPCC DPDown CCRight, (InChar, 12))
                                                                        , (DPCC DPUp CCLeft, (Pop, 0))
                                                                        , (DPCC DPUp CCRight, (Pop, 0))
                                                                        ]
                                                   )
                                                 , ( 7
                                                   , Block $ M.fromList [ (DPCC DPLeft CCLeft, (NoOperation, 9))
                                                                        , (DPCC DPLeft CCRight, (NoOperation, 9))
                                                                        , (DPCC DPUp CCLeft, (OutChar, 4))
                                                                        , (DPCC DPUp CCRight, (Roll, 5))
                                                                        ]
                                                   )
                                                 , ( 9
                                                   , Block $ M.fromList [ (DPCC DPDown CCLeft, (Push 16, 17))
                                                                        , (DPCC DPDown CCRight, (Push 16, 17))
                                                                        , (DPCC DPLeft CCLeft, (Switch, 12))
                                                                        , (DPCC DPLeft CCRight, (Switch, 12))
                                                                        , (DPCC DPUp CCLeft, (Duplicate, 1))
                                                                        , (DPCC DPUp CCRight, (Duplicate, 1))
                                                                        ]
                                                   )
                                                 , ( 12
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (Pointer, 9))
                                                                        , (DPCC DPRight CCRight, (Pointer, 9))
                                                                        , (DPCC DPDown CCLeft, (NoOperation, 23))
                                                                        , (DPCC DPUp CCLeft, (Add, 6))
                                                                        , (DPCC DPUp CCRight, (Add, 6))
                                                                        ]
                                                   )
                                                 , ( 15
                                                   , Block $ M.fromList [ (DPCC DPDown CCLeft, (Duplicate, 18))
                                                                        , (DPCC DPDown CCRight, (Duplicate, 18))
                                                                        , (DPCC DPLeft CCLeft, (Roll, 9))
                                                                        , (DPCC DPLeft CCRight, (Roll, 9))
                                                                        , (DPCC DPUp CCLeft, (Roll, 9))
                                                                        , (DPCC DPUp CCRight, (Roll, 9))
                                                                        ]
                                                   )
                                                 , ( 17
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (Push 1, 18))
                                                                        , (DPCC DPRight CCRight, (Push 1, 18))
                                                                        , (DPCC DPUp CCLeft, (Pop, 9))
                                                                        , (DPCC DPUp CCRight, (Pop, 9))
                                                                        ]
                                                   )
                                                 , ( 18
                                                   , Block $ M.fromList [ (DPCC DPDown CCLeft, (Divide, 25))
                                                                        , (DPCC DPDown CCRight, (Divide, 25))
                                                                        , (DPCC DPLeft CCLeft, (Pop, 17))
                                                                        , (DPCC DPLeft CCRight, (Pop, 17))
                                                                        , (DPCC DPUp CCLeft, (Divide, 15))
                                                                        ]
                                                   )
                                                 , ( 22
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (NoOperation, 23))
                                                                        , (DPCC DPRight CCRight, (NoOperation, 23))
                                                                        , (DPCC DPUp CCLeft, (NoOperation, 12))
                                                                        , (DPCC DPUp CCRight, (NoOperation, 12))
                                                                        ]
                                                   )
                                                 , ( 23
                                                   , Block $ M.fromList [ (DPCC DPRight CCLeft, (NoOperation, 25))
                                                                        , (DPCC DPRight CCRight, (NoOperation, 25))
                                                                        , (DPCC DPLeft CCLeft, (NoOperation, 22))
                                                                        , (DPCC DPLeft CCRight, (NoOperation, 22))
                                                                        , (DPCC DPUp CCLeft, (NoOperation, 12))
                                                                        , (DPCC DPUp CCRight, (NoOperation, 12))
                                                                        ]
                                                   )
                                                 , ( 25
                                                   , Block $ M.fromList [ (DPCC DPLeft CCLeft, (NoOperation, 23))
                                                                        , (DPCC DPLeft CCRight, (NoOperation, 23))
                                                                        , (DPCC DPUp CCLeft, (Duplicate, 18))
                                                                        , (DPCC DPUp CCRight, (Duplicate, 18))
                                                                        ]
                                                   )
                                                 ]
