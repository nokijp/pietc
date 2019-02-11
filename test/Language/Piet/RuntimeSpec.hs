{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Piet.RuntimeSpec
  ( main
  , spec
  ) where

import Control.Monad
import qualified Language.Piet.Runtime as P
import LLVMTestUtils
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "commands" $ do
    let pushRollOut xs = mapM_ P.push xs >> P.roll >> mapM_ (const P.outNumber) xs
    forM_
      [ ("pop", P.pop, "", "")

      , ("push 10; outNumber", P.push 10 >> P.outNumber, "", "10")

      , ("add; outNumber", P.add >> P.outNumber, "", "")
      , ("push 10; add; outNumber", P.push 10 >> P.add >> P.outNumber, "", "10")
      , ("push 10; push 2; add; outNumber", P.push 10 >> P.push 2 >> P.add >> P.outNumber, "", "12")

      , ("subtract; outNumber", P.subtract >> P.outNumber, "", "")
      , ("push 10; subtract; outNumber", P.push 10 >> P.subtract >> P.outNumber, "", "10")
      , ("push 10; push 2; subtract; outNumber", P.push 10 >> P.push 2 >> P.subtract >> P.outNumber, "", "8")

      , ("multiply; outNumber", P.multiply >> P.outNumber, "", "")
      , ("push 10; multiply; outNumber", P.push 10 >> P.multiply >> P.outNumber, "", "10")
      , ("push 10; push 2; multiply; outNumber", P.push 10 >> P.push 2 >> P.multiply >> P.outNumber, "", "20")

      , ("divide; outNumber", P.divide >> P.outNumber, "", "")
      , ("push 10; divide; outNumber", P.push 10 >> P.divide >> P.outNumber, "", "10")
      , ("push 11; push 3; divide; outNumber", P.push 11 >> P.push 3 >> P.divide >> P.outNumber, "", "3")
      , ("push 12; push 3; divide; outNumber", P.push 12 >> P.push 3 >> P.divide >> P.outNumber, "", "4")
      , ("push 13; push 3; divide; outNumber", P.push 13 >> P.push 3 >> P.divide >> P.outNumber, "", "4")
      , ("push -11; push 3; divide; outNumber", P.push (-11) >> P.push 3 >> P.divide >> P.outNumber, "", "-4")
      , ("push -12; push 3; divide; outNumber", P.push (-12) >> P.push 3 >> P.divide >> P.outNumber, "", "-4")
      , ("push -13; push 3; divide; outNumber", P.push (-13) >> P.push 3 >> P.divide >> P.outNumber, "", "-5")
      , ("push 11; push -3; divide; outNumber", P.push 11 >> P.push (-3) >> P.divide >> P.outNumber, "", "-4")
      , ("push 12; push -3; divide; outNumber", P.push 12 >> P.push (-3) >> P.divide >> P.outNumber, "", "-4")
      , ("push 13; push -3; divide; outNumber", P.push 13 >> P.push (-3) >> P.divide >> P.outNumber, "", "-5")
      , ("push -11; push -3; divide; outNumber", P.push (-11) >> P.push (-3) >> P.divide >> P.outNumber, "", "3")
      , ("push -12; push -3; divide; outNumber", P.push (-12) >> P.push (-3) >> P.divide >> P.outNumber, "", "4")
      , ("push -13; push -3; divide; outNumber", P.push (-13) >> P.push (-3) >> P.divide >> P.outNumber, "", "4")
      , ("push 12; push 0; divide; outNumber", P.push 12 >> P.push 0 >> P.divide >> P.outNumber >> P.outNumber, "", "012")

      , ("mod; outNumber", P.mod >> P.outNumber, "", "")
      , ("push 10; mod; outNumber", P.push 10 >> P.mod >> P.outNumber, "", "10")
      , ("push 11; push 3; mod; outNumber", P.push 11 >> P.push 3 >> P.mod >> P.outNumber, "", "2")
      , ("push 12; push 3; mod; outNumber", P.push 12 >> P.push 3 >> P.mod >> P.outNumber, "", "0")
      , ("push 13; push 3; mod; outNumber", P.push 13 >> P.push 3 >> P.mod >> P.outNumber, "", "1")
      , ("push -11; push 3; mod; outNumber", P.push (-11) >> P.push 3 >> P.mod >> P.outNumber, "", "1")
      , ("push -12; push 3; mod; outNumber", P.push (-12) >> P.push 3 >> P.mod >> P.outNumber, "", "0")
      , ("push -13; push 3; mod; outNumber", P.push (-13) >> P.push 3 >> P.mod >> P.outNumber, "", "2")
      , ("push 11; push -3; mod; outNumber", P.push 11 >> P.push (-3) >> P.mod >> P.outNumber, "", "-1")
      , ("push 12; push -3; mod; outNumber", P.push 12 >> P.push (-3) >> P.mod >> P.outNumber, "", "0")
      , ("push 13; push -3; mod; outNumber", P.push 13 >> P.push (-3) >> P.mod >> P.outNumber, "", "-2")
      , ("push -11; push -3; mod; outNumber", P.push (-11) >> P.push (-3) >> P.mod >> P.outNumber, "", "-2")
      , ("push -12; push -3; mod; outNumber", P.push (-12) >> P.push (-3) >> P.mod >> P.outNumber, "", "0")
      , ("push -13; push -3; mod; outNumber", P.push (-13) >> P.push (-3) >> P.mod >> P.outNumber, "", "-1")
      , ("push 12; push 0; mod; outNumber", P.push 12 >> P.push 0 >> P.mod >> P.outNumber >> P.outNumber, "", "012")

      , ("not; outNumber", P.not >> P.outNumber, "", "")
      , ("push -10; not; outNumber", P.push (-10) >> P.not >> P.outNumber, "", "0")
      , ("push 0; not; outNumber", P.push 0 >> P.not >> P.outNumber, "", "1")
      , ("push 10; not; outNumber", P.push 10 >> P.not >> P.outNumber, "", "0")

      , ("greater; outNumber", P.greater >> P.outNumber, "", "")
      , ("push 10; greater; outNumber", P.push 10 >> P.greater >> P.outNumber, "", "10")
      , ("push 1; push 2; greater; outNumber", P.push 1 >> P.push 2 >> P.greater >> P.outNumber, "", "0")
      , ("push 1; push 1; greater; outNumber", P.push 1 >> P.push 1 >> P.greater >> P.outNumber, "", "0")
      , ("push 2; push 1; greater; outNumber", P.push 2 >> P.push 1 >> P.greater >> P.outNumber, "", "1")

      -- pointer

      -- switch

      , ("duplicate; outNumber", P.duplicate >> P.outNumber, "", "")
      , ("push 1; duplicate; outNumber", P.push 1 >> P.duplicate >> P.outNumber >> P.outNumber >> P.outNumber, "", "11")

      , ("roll; outNumber", P.roll >> P.outNumber, "", "")
      , ("push 1; roll; outNumber", P.push 1 >> P.roll >> P.outNumber, "", "1")
      , ("push [1, 0]; roll; outChar*", pushRollOut [0, 0], "", "")
      , ("push [1, 0]; roll; outChar*", pushRollOut [1, 0], "", "01")
      , ("push [1, 1]; roll; outChar*", pushRollOut [1, 1], "", "11")
      , ("push [1, 2]; roll; outChar*", pushRollOut [1, 2], "", "21")
      , ("push [3, 0, 0]; roll; outChar*", pushRollOut [3, 0, 0], "", "3")
      , ("push [3, 0, 1]; roll; outChar*", pushRollOut [3, 0, 1], "", "3")
      , ("push [3, 1, 0]; roll; outChar*", pushRollOut [3, 1, 0], "", "3")
      , ("push [3, 1, 1]; roll; outChar*", pushRollOut [3, 1, 1], "", "3")
      , ("push [3, 2, 1]; roll; outChar*", pushRollOut [3, 2, 1], "", "123")
      , ("push [3, -1, 1]; roll; outChar*", pushRollOut [3, -1, 1], "", "1-13")
      , ("push [9, 8, 7, 6, 0, 0]; roll; outChar*", pushRollOut [9, 8, 7, 6, 0, 0], "", "6789")
      , ("push [9, 8, 7, 6, 3, -10]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, -10], "", "8679")
      , ("push [9, 8, 7, 6, 3, -4]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, -4], "", "8679")
      , ("push [9, 8, 7, 6, 3, -3]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, -3], "", "6789")
      , ("push [9, 8, 7, 6, 3, -2]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, -2], "", "7869")
      , ("push [9, 8, 7, 6, 3, -1]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, -1], "", "8679")
      , ("push [9, 8, 7, 6, 3, 0]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, 0], "", "6789")
      , ("push [9, 8, 7, 6, 3, 1]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, 1], "", "7869")
      , ("push [9, 8, 7, 6, 3, 2]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, 2], "", "8679")
      , ("push [9, 8, 7, 6, 3, 3]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, 3], "", "6789")
      , ("push [9, 8, 7, 6, 3, 4]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, 4], "", "7869")
      , ("push [9, 8, 7, 6, 3, 10]; roll; outChar*", pushRollOut [9, 8, 7, 6, 3, 10], "", "7869")
      , ("push [9, 8, 7, 6, 5, 4]; roll; outChar*", pushRollOut [9, 8, 7, 6, 5, 4], "", "456789")
      , ("push [9, 8, 7, 6, 5, 5]; roll; outChar*", pushRollOut [9, 8, 7, 6, 5, 5], "", "556789")

      , ("inNumber 10; outNumber", P.inNumber >> P.outNumber, "10", "10")
      , ("inNumber x; outNumber", P.inNumber >> P.outNumber, "x", "")
      , ("inNumber; outNumber", P.inNumber >> P.outNumber, "", "")

      , ("inChar A; outNumber", P.inChar >> P.outNumber, "A", "65")
      , ("inChar 10; outNumber", P.inChar >> P.outNumber, "10", "49")
      , ("inChar; outNumber", P.inChar >> P.outNumber, "", "")

      , ("push 65; outChar", P.push 65 >> P.outChar >> P.outNumber, "", "A")
      ] $ \(commandDescription, command, input, output) ->
        context ("when given commands: " ++ commandDescription) $ do
          actual <- runIO $ runAndCapture command input
          it "runs correctly" $ actual `shouldBe` output
