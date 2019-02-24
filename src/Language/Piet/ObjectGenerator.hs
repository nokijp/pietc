{-# LANGUAGE FlexibleContexts #-}

module Language.Piet.ObjectGenerator
  ( generateExecutable
  , generateObject
  ) where

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Language.Piet.Runtime as P
import qualified LLVM.AST as AST
import LLVM.Context
import LLVM.Module
import LLVM.Target
import System.Directory
import System.Exit
import System.IO
import System.Process

data ObjectGeneratorError = CompileError String
                          | LinkError String
                          | TempFileError String
                            deriving (Show, Eq)

generateExecutable :: (MonadIO m, MonadError ObjectGeneratorError m) => FilePath -> AST.Module -> m ()
generateExecutable outPath ast = do
  objectString <- generateObject ast
  link outPath objectString

generateObject :: (MonadIO m, MonadError ObjectGeneratorError m) => AST.Module -> m ByteString
generateObject ast = captureException (\e -> CompileError $ show (e :: SomeException)) generate where
  generate =
    withContext $ \ctx ->
      withModuleFromLLVMAssembly ctx P.runtimeAssembly $ \runtimeMod ->
        withModuleFromAST ctx ast $ \astMod ->
          withHostTargetMachine $ \targetMachine -> do
            linkModules astMod runtimeMod
            let linkedModule = astMod
            moduleObject targetMachine linkedModule

link :: (MonadIO m, MonadError ObjectGeneratorError m) => FilePath -> ByteString -> m ()
link outPath objectString = toM $ catchTempFileError $ try $ linkIO where
  toM :: (MonadIO m, MonadError e m) => IO (Either e a) -> m a
  toM x = liftIO x >>= either throwError return
  catchTempFileError :: IO (Either SomeException (Either ObjectGeneratorError ())) -> IO (Either ObjectGeneratorError ())
  catchTempFileError = fmap (join . left (TempFileError . show))
  linkIO :: IO (Either ObjectGeneratorError ())
  linkIO = withObjectFile objectString $ \path -> do
    (exitCode, _, stdErrString) <- readProcessWithExitCode "/usr/bin/ld" ["-lc", "-o", outPath, path] ""
    return $ if exitCode == ExitSuccess then Right () else Left $ LinkError stdErrString

withObjectFile :: ByteString -> (FilePath -> IO a) -> IO a
withObjectFile objectString = bracket createTempFile removeTempFile where
  createTempFile = do
    tempDir <- getTemporaryDirectory
    (path, fHandle) <- openTempFile tempDir "pietobj.o"
    bracket (return fHandle) hClose writeObject
    return path
  writeObject fHandle = B.hPut fHandle objectString
  removeTempFile = removeFile

captureException :: (MonadIO m, MonadError c m, Exception b) => (b -> c) -> IO a -> m a
captureException f = (either (throwError . f) return =<<) . liftIO . try
