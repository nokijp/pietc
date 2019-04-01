module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Except
import Data.Version
import qualified Data.Text.Lazy.IO as T
import ErrorMessage
import Language.Piet
import OptParser
import Paths_pietc
import VerboseMessage

main :: IO ()
main = do
  mode <- parseArgs
  runProgram mode

runProgram :: ProgramMode -> IO ()
runProgram ShowVersionInfo = putStrLn $ "pietc " ++ showVersion version
runProgram (RunCompiler config) = runCompiler config

runCompiler :: CompilerConfig -> IO ()
runCompiler (OutputBinaryConfig inputFile outputFile imageConfig optimizationLevel isVerbose) =
  toIO $ compile (receiver isVerbose) imageConfig optimizationLevel inputFile outputFile
runCompiler (RunJITConfig inputFile imageConfig optimizationLevel isVerbose) =
  toIO $ run (receiver isVerbose) imageConfig optimizationLevel inputFile
runCompiler (OutputGraphConfig inputFile imageConfig isVerbose) =
  toIO $ printGraphText isVerbose imageConfig inputFile

toIO :: ExceptT PietError IO () -> IO ()
toIO = either printError return <=< runExceptT

printGraphText :: Bool -> ImageConfig -> FilePath -> ExceptT PietError IO ()
printGraphText isVerbose imageConfig inputPath =
  lift . T.putStrLn =<< graphText (receiver isVerbose) imageConfig inputPath

receiver :: MonadIO m => Bool -> PietStep -> m ()
receiver False = nullReceiver
receiver True = liftIO . printVerboseMessage
