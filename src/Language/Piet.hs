{-# LANGUAGE FlexibleContexts #-}

module Language.Piet
  ( PietError(..)
  , compile
  , run
  , graphText

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

-- | Compile a Piet program.
compile :: ( MonadIO m
           , MonadError PietError m
           )
        => ImageConfig
        -> OptimizationLevel
        -> FilePath
        -> FilePath
        -> m ()
compile imageConfig optimizationLevel inputPath outputPath = do
  ast <- makeAST imageConfig inputPath
  mapError PietObjectGeneratorError $ generateExecutable optimizationLevel outputPath ast

-- | Run a Piet program on JIT.
run :: ( MonadIO m
       , MonadError PietError m
       )
    => ImageConfig
    -> OptimizationLevel
    -> FilePath
    -> m ()
run imageConfig optimizationLevel inputPath = do
  ast <- makeAST imageConfig inputPath
  liftIO $ runJIT optimizationLevel ast

-- | Convert a Piet program to a graph script.
graphText :: ( MonadIO m
             , MonadError PietError m
             )
          => ImageConfig
          -> FilePath
          -> m Text
graphText imageConfig inputPath =
  syntaxToDOT <$> makeGraph imageConfig inputPath

makeAST :: ( MonadIO m
           , MonadError PietError m
           )
        => ImageConfig
        -> FilePath
        -> m AST.Module
makeAST imageConfig inputPath = do
  graph <- makeGraph imageConfig inputPath
  return $ generateAssembly graph

makeGraph :: ( MonadIO m
             , MonadError PietError m
             )
          => ImageConfig
          -> FilePath
          -> m SyntaxGraph
makeGraph imageConfig inputPath = do
  codels <- mapError PietImageReaderError $ readCodels imageConfig inputPath
  mapError PietParserError $ parse codels

mapError :: MonadError e2 m => (e1 -> e2) -> ExceptT e1 m a -> m a
mapError f = either (throwError . f) return <=< runExceptT
