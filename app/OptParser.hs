module OptParser
  ( RunType(..)
  , ProgramConfig(..)
  , parseArgs
  ) where

import Data.Maybe
import Language.Piet
import Options.Applicative

data ProgramConfig = ProgramConfig { runType :: RunType
                                   , inputFile :: FilePath
                                   , imageConfig :: ImageConfig
                                   , optimizationLevel :: OptimizationLevel
                                   }

data RunType = OutputBinary { _outputFile :: String } | RunJIT

parseArgs :: IO ProgramConfig
parseArgs = execParser parserInfo

parserInfo :: ParserInfo ProgramConfig
parserInfo = info (parser <**> helper) fullDesc

parser :: Parser ProgramConfig
parser = toConfig <$> optional (option auto $ long "codel-size" <> metavar "<size>" <> help "Set size of codel")
                  <*> optional (option additionalColorOptReader $ long "additional" <> metavar "<type>" <> help "Set method to deal with additional colors")
                  <*> optional (option multicoloredCodelOptReader $ long "multicolor" <> metavar "<type>" <> help "Set method to deal with multicolored codels")
                  <*> optional (option optimizationLevelOptReader $ short 'O' <> metavar "<level>" <> help "Set optimization level")
                  <*> (   (fmap OutputBinary $ strOption $ short 'o' <> metavar "<file>" <> help "Write output to <file>")
                      <|> (flag' RunJIT $ long "run" <> help "Run program without generating binaries")
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
  toConfig codelSizeOpt additionalColorOpt multicoloredCodelOpt optimizationLevelOpt runTypeOpt inputOpt = config where
    config = ProgramConfig { runType = runTypeOpt
                           , inputFile = inputOpt
                           , imageConfig = imageConfigOpt
                           , optimizationLevel = fromMaybe OptimizationLevelMiddle optimizationLevelOpt
                           }
    imageConfigOpt = ImageConfig { additionalColor = fromMaybe (additionalColor defaultImageConfig) additionalColorOpt
                                 , multicoloredCodel = fromMaybe (multicoloredCodel defaultImageConfig) multicoloredCodelOpt
                                 , codelSize = fromMaybe (codelSize defaultImageConfig) codelSizeOpt
                                 }
