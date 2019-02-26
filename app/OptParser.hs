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
                  <*> (   (fmap OutputBinary $ strOption $ short 'o' <> metavar "<file>" <> help "Write output to <file>")
                      <|> (flag' RunJIT $ long "run" <> help "Run program without generating binaries")
                      )
                  <*> strArgument (metavar "<input>") where
  additionalColorOptReader = eitherReader f where
    f "white" = Right AdditionalColorAsWhite
    f "black" = Right AdditionalColorAsBlack
    f "nearest" = Right AdditionalColorNearest
    f _ = Left $ "accepts only `white', `black' or `nearest'"
  multicoloredCodelOptReader = eitherReader f where
    f "white" = Right MulticoloredCodelAsWhite
    f "black" = Right MulticoloredCodelAsBlack
    f "center" = Right MulticoloredCodelCenter
    f "modal" = Right MulticoloredCodelModal
    f "average" = Right MulticoloredCodelAverage
    f _ = Left $ "accepts only `white', `black', `center', `modal' or `average'"
  toConfig codelSizeOpt additionalColorOpt multicoloredCodelOpt runTypeOpt inputOpt = config where
    config = ProgramConfig { runType = runTypeOpt
                           , inputFile = inputOpt
                           , imageConfig = imageConfigOpt
                           }
    imageConfigOpt = ImageConfig { additionalColor = fromMaybe (additionalColor defaultImageConfig) additionalColorOpt
                                 , multicoloredCodel = fromMaybe (multicoloredCodel defaultImageConfig) multicoloredCodelOpt
                                 , codelSize = fromMaybe (codelSize defaultImageConfig) codelSizeOpt
                                 }
