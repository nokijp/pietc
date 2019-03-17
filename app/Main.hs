module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Except
import qualified Data.Text.Lazy.IO as T
import ErrorMessage
import Language.Piet
import OptParser
import VerboseMessage

main :: IO ()
main = do
  config <- parseArgs
  runProgram config

runProgram :: ProgramConfig -> IO ()
runProgram (OutputBinaryConfig inputFile outputFile imageConfig optimizationLevel isVerbose) =
  toIO $ compile (receiver isVerbose) imageConfig optimizationLevel inputFile outputFile
runProgram (RunJITConfig inputFile imageConfig optimizationLevel isVerbose) =
  toIO $ run (receiver isVerbose) imageConfig optimizationLevel inputFile
runProgram (OutputGraphConfig inputFile imageConfig isVerbose) =
  toIO $ printGraphText isVerbose imageConfig inputFile

toIO :: ExceptT PietError IO () -> IO ()
toIO = either printError return <=< runExceptT

printGraphText :: Bool -> ImageConfig -> FilePath -> ExceptT PietError IO ()
printGraphText isVerbose imageConfig inputPath =
  lift . T.putStrLn =<< graphText (receiver isVerbose) imageConfig inputPath

receiver :: MonadIO m => Bool -> PietStep -> m ()
receiver False = nullReceiver
receiver True = liftIO . printVerboseMessage
