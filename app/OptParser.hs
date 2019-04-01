module OptParser
  ( ProgramMode(..)
  , CompilerConfig(..)
  , parseArgs
  ) where

import Data.Maybe
import Language.Piet
import Options.Applicative

data ProgramMode = ShowVersionInfo | RunCompiler CompilerConfig

data CompilerConfig = OutputBinaryConfig { _inputFile :: FilePath
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

parseArgs :: IO ProgramMode
parseArgs = execParser parserInfo

parserInfo :: ParserInfo ProgramMode
parserInfo = info (parser <**> helper) fullDesc

parser :: Parser ProgramMode
parser = (ShowVersionInfo <$ versionOption) <|> (RunCompiler <$> compilerConfigParser) where
  versionOption = flag' () $ long "version" <> help "Show version information" <> hidden

compilerConfigParser :: Parser CompilerConfig
compilerConfigParser = toConfig <$> verboseOption
                                <*> optional codelSizeOption
                                <*> optional additionalColorOption
                                <*> optional multicoloredCodelOption
                                <*> optional optimizationLevelOption
                                <*> (outputBinaryOption <|> runJITOption <|> outputGraphOption)
                                <*> inputFileArgument where
  verboseOption = switch $ long "verbose" <> short 'v' <> help "Use verbose output" <> hidden
  codelSizeOption = option auto $ long "codel-size" <> metavar "<size>" <> help "Set size of codel (default: guess)" <> hidden
  additionalColorOption = option additionalColorOptReader $ long "additional" <> metavar "<type>" <> help "Set method to deal with additional colors (default: nearest)" <> hidden
  multicoloredCodelOption = option multicoloredCodelOptReader $ long "multicolor" <> metavar "<type>" <> help "Set method to deal with multicolored codels (default: average)" <> hidden
  optimizationLevelOption = option optimizationLevelOptReader $ short 'O' <> metavar "<level>" <> help "Set optimization level (default: 2)" <> hidden
  inputFileArgument = strArgument $ metavar "<input>"
  outputBinaryOption = OutputBinary <$> strOption (short 'o' <> metavar "<file>" <> help "Write output to <file>")
  runJITOption = flag' RunJIT $ long "run" <> help "Run program without generating binaries"
  outputGraphOption = flag' OutputGraph $ long "graph" <> help "Print syntax graph in DOT format"

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
