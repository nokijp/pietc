{-# LANGUAGE OverloadedStrings #-}

module LLVMTestUtils
  ( runAndCapture
  , capture
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Language.Piet.CompileOption
import Language.Piet.Internal.LLVM
import qualified LLVM.AST as AST
import LLVM.Linking
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import LLVM.Target
import TestUtils

foreign import ccall "replace_stdin" replaceStdin :: CString -> IO (Ptr CFile)
foreign import ccall "replace_stdout" replaceStdout :: CString -> IO (Ptr CFile)
foreign import ccall "restore_stdin" restoreStdin :: Ptr CFile -> IO ()
foreign import ccall "restore_stdout" restoreStdout :: Ptr CFile -> IO ()

foreign import ccall "dynamic" mkIntFunction :: FunPtr (Int -> IO Int) -> Int -> IO Int

runAndCapture :: AST.Module -> ShortByteString -> (Int, ByteString) -> IO (Int, ByteString)
runAndCapture ast symbol = capture $ runModule mkIntFunction ast symbol

capture :: (Int -> IO Int) -> (Int, ByteString) -> IO (Int, ByteString)
capture f (arg, input) = do
  withTempFile "pietc-llvm-input" $ \inputFilePath ->
    withTempFile "pietc-llvm-input" $ \outputFilePath -> do
      BS.writeFile inputFilePath input
      res <- withCString inputFilePath $ \inputFilePathCStr ->
        withCString outputFilePath $ \outputFilePathCStr ->
          bracket (replaceStdin inputFilePathCStr) restoreStdin $ \_ ->
            bracket (replaceStdout outputFilePathCStr) restoreStdout $ \_ ->
              f arg
      s <- BS.readFile outputFilePath
      return (res, s)

runModule :: (FunPtr (a -> IO b) -> a -> IO b) -> AST.Module -> ShortByteString -> a -> IO b
runModule mkFunction ast symbol x = do
  _ <- loadLibraryPermanently Nothing
  initializeNativeTarget
  withLinkedModule NoOptimization ast $ \linkedModule ->
    withHostDynamicJITTargetMachine $ \targetMachine ->
      withObjectLinkingLayer $ \objectLayer ->
        withIRCompileLayer objectLayer targetMachine $ \compileLayer ->
          withModule compileLayer linkedModule (resolver compileLayer) $ \_ -> do
            mangledSymbol <- mangleSymbol compileLayer symbol
            Right (JITSymbol funcPtr _) <- findSymbol compileLayer mangledSymbol True
            mkFunction (castPtrToFunPtr $ wordPtrToPtr funcPtr) x
