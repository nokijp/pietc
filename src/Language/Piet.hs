module Language.Piet
  ( PietError(..)
  , compile

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
import Language.Piet.ImageReader
import Language.Piet.ObjectGenerator
import Language.Piet.Parser

data PietError = PietImageReaderError ImageReaderError
               | PietParserError ParserError
               | PietObjectGeneratorError ObjectGeneratorError
                 deriving (Show, Eq)

compile :: ImageConfig -> FilePath -> FilePath -> ExceptT PietError IO ()
compile imageConfig inputPath outputPath = do
  codels <- withExceptT PietImageReaderError $ readCodels imageConfig inputPath
  graph <- withExceptT PietParserError $ parse codels
  let ast = generateAssembly graph
  withExceptT PietObjectGeneratorError $ generateExecutable outputPath ast
