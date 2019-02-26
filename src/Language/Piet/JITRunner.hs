{-# LANGUAGE OverloadedStrings #-}

module Language.Piet.JITRunner
  ( runJIT
  ) where

import Foreign.Ptr
import Language.Piet.Internal.LLVM
import qualified LLVM.AST as AST
import LLVM.Linking
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import LLVM.Target

foreign import ccall "dynamic" mkMainFunction :: FunPtr (IO Int) -> IO Int

runJIT :: AST.Module -> IO ()
runJIT ast = do
  _ <- loadLibraryPermanently Nothing
  initializeNativeTarget
  withLinkedModule ast $ \linkedModule ->
    withHostDynamicJITTargetMachine $ \targetMachine ->
      withObjectLinkingLayer $ \objectLayer ->
        withIRCompileLayer objectLayer targetMachine $ \compileLayer ->
          withModule compileLayer linkedModule (resolver compileLayer) $ \_ -> do
            mangledSymbol <- mangleSymbol compileLayer "main"
            Right (JITSymbol funcPtr _) <- findSymbol compileLayer mangledSymbol True
            _ <- mkMainFunction $ castPtrToFunPtr $ wordPtrToPtr funcPtr
            return ()
