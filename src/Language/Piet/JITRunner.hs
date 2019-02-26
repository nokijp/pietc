{-# LANGUAGE OverloadedStrings #-}

module Language.Piet.JITRunner
  ( runJIT
  ) where

import Foreign.Ptr
import qualified Language.Piet.Runtime as P
import qualified LLVM.AST as AST
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import LLVM.Context
import LLVM.Linking
import LLVM.Module
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import qualified LLVM.Relocation as Reloc
import LLVM.Target

foreign import ccall "dynamic" mkMainFunction :: FunPtr (IO Int) -> IO Int

runJIT :: AST.Module -> IO ()
runJIT ast = do
  _ <- loadLibraryPermanently Nothing
  initializeNativeTarget
  withContext $ \ctx ->
    withModuleFromLLVMAssembly ctx P.runtimeAssembly $ \runtimeMod ->
      withModuleFromAST ctx ast $ \astMod ->
        withHostDynamicJITTargetMachine $ \targetMachine ->
          withObjectLinkingLayer $ \objectLayer ->
            withIRCompileLayer objectLayer targetMachine $ \compileLayer ->
              withModule compileLayer runtimeMod (resolver compileLayer) $ \_ ->
                withModule compileLayer astMod (resolver compileLayer) $ \_ -> do
                  mangledSymbol <- mangleSymbol compileLayer "main"
                  Right (JITSymbol funcPtr _) <- findSymbol compileLayer mangledSymbol True
                  _ <- mkMainFunction $ castPtrToFunPtr $ wordPtrToPtr funcPtr
                  return ()

resolver :: IRCompileLayer l -> SymbolResolver
resolver cl = SymbolResolver dylibResolver' externalResolver' where
  dylibResolver' sym = findSymbol cl sym True
  externalResolver' sym = do
    addr <- getSymbolAddressInProcess sym
    return $ return $ JITSymbol addr defaultJITSymbolFlags

withHostDynamicJITTargetMachine :: (TargetMachine -> IO a) -> IO a
withHostDynamicJITTargetMachine f = do
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.Default CodeModel.JITDefault CodeGenOpt.Default f
