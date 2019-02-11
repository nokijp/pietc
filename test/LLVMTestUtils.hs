{-# LANGUAGE OverloadedStrings #-}

module LLVMTestUtils
  ( runAndCapture
  ) where

import Control.Exception hiding (handle)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Language.Piet.Runtime as P
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import LLVM.Context
import LLVM.IRBuilder
import LLVM.Linking
import LLVM.Module
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import qualified LLVM.Relocation as Reloc
import LLVM.Target
import System.Directory
import System.IO

foreign import ccall "replace_stdin" replaceStdin :: CString -> IO (Ptr CFile)
foreign import ccall "replace_stdout" replaceStdout :: CString -> IO (Ptr CFile)
foreign import ccall "restore_stdin" restoreStdin :: Ptr CFile -> IO ()
foreign import ccall "restore_stdout" restoreStdout :: Ptr CFile -> IO ()

runAndCapture :: IRBuilderT ModuleBuilder () -> ByteString -> IO ByteString
runAndCapture command input = do
  let ast = buildModule "" $ do
              P.declareFunctions
              _ <- function "f" [] T.void $ \_ -> do
                command
                P.resetStack
                retVoid
              return ()
  withTempFile $ \inputFilePath ->
    withTempFile $ \outputFilePath -> do
      BS.writeFile inputFilePath input
      withCString inputFilePath $ \inputFilePathCStr ->
        withCString outputFilePath $ \outputFilePathCStr ->
          bracket (replaceStdin inputFilePathCStr) restoreStdin $ \_ ->
            bracket (replaceStdout outputFilePathCStr) restoreStdout $ \_ ->
              runModule ast "f"
      BS.readFile outputFilePath

withTempFile :: (FilePath -> IO a) -> IO a
withTempFile = bracket createTempFile removeTempFile where
  createTempFile = do
    tempDir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempDir "pietc-llvm"
    hClose handle
    return path
  removeTempFile = removeFile

foreign import ccall "dynamic" mkFunction :: FunPtr (IO ()) -> IO ()

runModule :: AST.Module -> ShortByteString -> IO ()
runModule ast symbol = do
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
                  mkFunction $ castPtrToFunPtr $ wordPtrToPtr funcPtr

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
