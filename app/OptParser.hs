module OptParser
  ( ProgramConfig(..)
  , parseArgs
  ) where

import Data.Maybe
import Language.Piet
import Options.Applicative

data ProgramConfig = OutputBinaryConfig { _inputFile :: FilePath
                                        , _outputFile :: FilePath
                                        , _imageConfig :: ImageConfig
                                        , _optimizationLevel :: OptimizationLevel
                                        , _isVerbose :: Bool
                                        }
                   | RunJITConfig { _inputFile :: FilePath
                                  , _imageConfig :: ImageConfig
                                  , _optimizationLevel :: OptimizationLevel
                                  , _isVerbose :: Bool
                                  }
                   | OutputGraphConfig { _inputFile :: FilePath
                                       , _imageConfig :: ImageConfig
                                       , _isVerbose :: Bool
                                       }

data RunType = OutputBinary { __outputFile :: String } | RunJIT | OutputGraph

parseArgs :: IO ProgramConfig
parseArgs = execParser parserInfo

parserInfo :: ParserInfo ProgramConfig
parserInfo = info (parser <**> helper) fullDesc

parser :: Parser ProgramConfig
parser = toConfig <$> switch (long "verbose" <> short 'v' <> help "Use verbose output")
                  <*> optional (option auto $ long "codel-size" <> metavar "<size>" <> help "Set size of codel (default: guess)")
                  <*> optional (option additionalColorOptReader $ long "additional" <> metavar "<type>" <> help "Set method to deal with additional colors (default: nearest)")
                  <*> optional (option multicoloredCodelOptReader $ long "multicolor" <> metavar "<type>" <> help "Set method to deal with multicolored codels (default: average)")
                  <*> optional (option optimizationLevelOptReader $ short 'O' <> metavar "<level>" <> help "Set optimization level (default: 2)")
                  <*> (   OutputBinary <$> strOption (short 'o' <> metavar "<file>" <> help "Write output to <file>")
                      <|> flag' RunJIT (long "run" <> help "Run program without generating binaries")
                      <|> flag' OutputGraph (long "graph" <> help "Print syntax graph in DOT format")
                      )
                  <*> strArgument (metavar "<input>") where
  additionalColorOptReader = eitherReader f where
    f "white"   = Right AdditionalColorAsWhite
    f "black"   = Right AdditionalColorAsBlack
    f "nearest" = Right AdditionalColorNearest
    f _         = Left "accepts only `white', `black' or `nearest'"
  multicoloredCodelOptReader = eitherReader f where
    f "white"   = Right MulticoloredCodelAsWhite
    f "black"   = Right MulticoloredCodelAsBlack
    f "center"  = Right MulticoloredCodelCenter
    f "modal"   = Right MulticoloredCodelModal
    f "average" = Right MulticoloredCodelAverage
    f _         = Left "accepts only `white', `black', `center', `modal' or `average'"
  optimizationLevelOptReader = eitherReader f where
    f "0" = Right NoOptimization
    f "1" = Right OptimizationLevelLow
    f "2" = Right OptimizationLevelMiddle
    f "3" = Right OptimizationLevelHigh
    f "s" = Right SizeLevelLow
    f "z" = Right SizeLevelHigh
    f _   = Left "accepts only `0', `1', `2', `3', `s' or `z'"
  toConfig isVerboseOpt codelSizeIntOpt additionalColorOpt multicoloredCodelOpt optimizationLevelOpt runTypeOpt inputOpt = config runTypeOpt where
    config (OutputBinary path) = OutputBinaryConfig inputOpt path imageConfigOpt (fromMaybe OptimizationLevelMiddle optimizationLevelOpt) isVerboseOpt
    config RunJIT = RunJITConfig inputOpt imageConfigOpt (fromMaybe OptimizationLevelMiddle optimizationLevelOpt) isVerboseOpt
    config OutputGraph = OutputGraphConfig inputOpt imageConfigOpt isVerboseOpt
    imageConfigOpt = ImageConfig (fromMaybe AdditionalColorNearest additionalColorOpt)
                                 (fromMaybe MulticoloredCodelAverage multicoloredCodelOpt)
                                 (maybe GuessCodelSize CodelSize codelSizeIntOpt)
