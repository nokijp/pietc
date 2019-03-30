{-# LANGUAGE OverloadedStrings #-}

module Language.Piet.ObjectGeneratorSpec
  ( main
  , spec
  ) where

import Control.Monad.Except
import Data.Char
import Language.Piet.CompileOption
import Language.Piet.ObjectGenerator
import qualified Language.Piet.Internal.Runtime as P
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import LLVM.IRBuilder
import System.Process
import Test.Hspec
import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "generateExecutable" $
    context "when given a simple AST" $ do
      output <- runIO $ withTempFile "pietc-executable" $ \executablePath -> do
        Right () <- runExceptT $ generateExecutable NoOptimization executablePath simpleAST
        callCommand "ls -lh /tmp/"
        readProcess executablePath [] ""
      it "generates an executable" $ output `shouldBe` "hello"

simpleAST :: AST.Module
simpleAST = buildModule "" $ do
  P.declareFunctions
  _ <- function "main" [] T.i32 $ \_ -> do
    mapM_ (\c -> P.push (ord c) >> P.outChar) ("hello" :: String)
    P.resetStack
    ret $ AST.ConstantOperand $ C.Int 32 0
  return ()
