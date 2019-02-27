{-# LANGUAGE OverloadedStrings #-}

module Language.Piet.JITRunnerSpec
  ( main
  , spec
  ) where

import Data.Char
import Data.Functor
import Language.Piet.CompileOption
import Language.Piet.JITRunner
import qualified Language.Piet.Runtime as P
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import LLVM.IRBuilder
import LLVMTestUtils
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runJIT" $
    context "when given a simple AST" $ do
      (_, output) <- runIO $ capture (const $ runJIT NoOptimization simpleAST $> 0) (0, "")
      it "runs a module on JIT" $ output `shouldBe` "hello"

simpleAST :: AST.Module
simpleAST = buildModule "" $ do
  P.declareFunctions
  _ <- function "main" [] T.i32 $ \_ -> do
    mapM_ (\c -> P.push (ord c) >> P.outChar) ("hello" :: String)
    P.resetStack
    ret $ AST.ConstantOperand $ C.Int 32 0
  return ()
