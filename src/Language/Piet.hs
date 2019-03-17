{-# LANGUAGE FlexibleContexts #-}

module Language.Piet
  ( PietError(..)
  , PietStep(..)
  , compile
  , run
  , graphText
  , nullReceiver

  , OptimizationLevel(..)

  , ImageReaderError(..)
  , AdditionalColorStrategy(..)
  , MulticoloredCodelStrategy(..)
  , ImageConfig(..)
  , CodelSize(..)

  , ObjectGeneratorError(..)

  , ParserError(..)
  ) where

import Control.Monad.Except
import Data.Text.Lazy (Text)
import Language.Piet.AssemblyGenerator
import Language.Piet.CompileOption
import Language.Piet.ImageReader
import Language.Piet.JITRunner
import Language.Piet.ObjectGenerator
import Language.Piet.Parser
import Language.Piet.Syntax
import Language.Piet.SyntaxVisualizer
import qualified LLVM.AST as AST

data PietError = PietImageReaderError ImageReaderError
               | PietParserError ParserError
               | PietObjectGeneratorError ObjectGeneratorError
                 deriving (Show, Eq)

data PietStep = StepReadImage
              | StepParse
              | StepMakeAssembly
              | StepGenerateExecutable
              | StepRunJIT
              | StepGenerateDOT
                deriving (Show, Eq)

-- | Compile a Piet program.
compile :: ( MonadIO m
           , MonadError PietError m
           )
        => (PietStep -> m ())
        -> ImageConfig
        -> OptimizationLevel
        -> FilePath
        -> FilePath
        -> m ()
compile messageReceiver imageConfig optimizationLevel inputPath outputPath = do
  ast <- makeAST messageReceiver imageConfig inputPath
  messageReceiver StepGenerateExecutable
  mapError PietObjectGeneratorError $ generateExecutable optimizationLevel outputPath ast

-- | Run a Piet program on JIT.
run :: ( MonadIO m
       , MonadError PietError m
       )
    => (PietStep -> m ())
    -> ImageConfig
    -> OptimizationLevel
    -> FilePath
    -> m ()
run messageReceiver imageConfig optimizationLevel inputPath = do
  ast <- makeAST messageReceiver imageConfig inputPath
  messageReceiver StepRunJIT
  liftIO $ runJIT optimizationLevel ast

-- | Convert a Piet program to a graph script.
graphText :: ( MonadIO m
             , MonadError PietError m
             )
          => (PietStep -> m ())
          -> ImageConfig
          -> FilePath
          -> m Text
graphText messageReceiver imageConfig inputPath = do
  graph <- makeGraph messageReceiver imageConfig inputPath
  messageReceiver StepGenerateDOT
  return $ syntaxToDOT graph

makeAST :: ( MonadIO m
           , MonadError PietError m
           )
        => (PietStep -> m ())
        -> ImageConfig
        -> FilePath
        -> m AST.Module
makeAST messageReceiver imageConfig inputPath = do
  graph <- makeGraph messageReceiver imageConfig inputPath
  messageReceiver StepMakeAssembly
  return $ generateAssembly graph

makeGraph :: ( MonadIO m
             , MonadError PietError m
             )
          => (PietStep -> m ())
          -> ImageConfig
          -> FilePath
          -> m SyntaxGraph
makeGraph messageReceiver imageConfig inputPath = do
  messageReceiver StepReadImage
  codels <- mapError PietImageReaderError $ readCodels imageConfig inputPath
  messageReceiver StepParse
  mapError PietParserError $ parse codels

nullReceiver :: Monad m => PietStep -> m ()
nullReceiver _ = return ()

mapError :: MonadError e2 m => (e1 -> e2) -> ExceptT e1 m a -> m a
mapError f = either (throwError . f) return <=< runExceptT
