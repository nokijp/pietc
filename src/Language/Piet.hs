module Language.Piet
  ( PietError(..)
  , compile
  , run

  , OptimizationLevel(..)

  , ImageReaderError(..)
  , AdditionalColorStrategy(..)
  , MulticoloredCodelStrategy(..)
  , ImageConfig(..)
  , defaultImageConfig

  , ObjectGeneratorError(..)

  , ParserError(..)
  ) where

import Control.Monad.Except
import Language.Piet.AssemblyGenerator
import Language.Piet.CompileOption
import Language.Piet.ImageReader
import Language.Piet.JITRunner
import Language.Piet.ObjectGenerator
import Language.Piet.Parser
import qualified LLVM.AST as AST

data PietError = PietImageReaderError ImageReaderError
               | PietParserError ParserError
               | PietObjectGeneratorError ObjectGeneratorError
                 deriving (Show, Eq)

compile :: ImageConfig -> OptimizationLevel -> FilePath -> FilePath -> ExceptT PietError IO ()
compile imageConfig optimizationLevel inputPath outputPath = do
  ast <- makeAST imageConfig inputPath
  withExceptT PietObjectGeneratorError $ generateExecutable optimizationLevel outputPath ast

run :: ImageConfig -> OptimizationLevel -> FilePath -> ExceptT PietError IO ()
run imageConfig optimizationLevel inputPath = do
  ast <- makeAST imageConfig inputPath
  lift $ runJIT optimizationLevel ast

makeAST :: ImageConfig -> FilePath -> ExceptT PietError IO AST.Module
makeAST imageConfig inputPath = do
  codels <- withExceptT PietImageReaderError $ readCodels imageConfig inputPath
  graph <- withExceptT PietParserError $ parse codels
  return $ generateAssembly graph
