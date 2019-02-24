{-# LANGUAGE OverloadedStrings #-}

module LLVMTestUtils
  ( runAndCapture
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import Foreign.C.String
import Foreign.C.Types
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
import TestUtils

foreign import ccall "replace_stdin" replaceStdin :: CString -> IO (Ptr CFile)
foreign import ccall "replace_stdout" replaceStdout :: CString -> IO (Ptr CFile)
foreign import ccall "restore_stdin" restoreStdin :: Ptr CFile -> IO ()
foreign import ccall "restore_stdout" restoreStdout :: Ptr CFile -> IO ()

foreign import ccall "dynamic" mkIntFunction :: FunPtr (Int -> IO Int) -> Int -> IO Int

runAndCapture :: AST.Module -> ShortByteString -> (Int, ByteString) -> IO (Int, ByteString)
runAndCapture ast symbol (arg, input) = do
  withTempFile "pietc-llvm-input" $ \inputFilePath ->
    withTempFile "pietc-llvm-input" $ \outputFilePath -> do
      BS.writeFile inputFilePath input
      res <- withCString inputFilePath $ \inputFilePathCStr ->
        withCString outputFilePath $ \outputFilePathCStr ->
          bracket (replaceStdin inputFilePathCStr) restoreStdin $ \_ ->
            bracket (replaceStdout outputFilePathCStr) restoreStdout $ \_ ->
              runModule mkIntFunction ast symbol arg
      s <- BS.readFile outputFilePath
      return (res, s)

runModule :: (FunPtr (a -> IO b) -> a -> IO b) -> AST.Module -> ShortByteString -> a -> IO b
runModule mkFunction ast symbol x = do
  _ <- loadLibraryPermanently Nothing
  initializeNativeTarget
  withContext $ \ctx ->
    withModuleFromLLVMAssembly ctx P.runtimeAssembly $ \runtimeMod ->
      withModuleFromAST ctx ast $ \astMod ->
        withHostDynamicJITTargetMachine $ \targetMachine ->
          withObjectLinkingLayer $ \objectLayer ->
            withIRCompileLayer objectLayer targetMachine $ \compileLayer ->
              withModule compileLayer runtimeMod (resolver compileLayer) $ \_ -> do
                withModule compileLayer astMod (resolver compileLayer) $ \_ -> do
                  mangledSymbol <- mangleSymbol compileLayer symbol
                  Right (JITSymbol funcPtr _) <- findSymbol compileLayer mangledSymbol True
                  mkFunction (castPtrToFunPtr $ wordPtrToPtr funcPtr) x

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
