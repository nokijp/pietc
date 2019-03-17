module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Except
import qualified Data.Text.Lazy.IO as T
import ErrorMessage
import Language.Piet
import OptParser

main :: IO ()
main = do
  config <- parseArgs
  runProgram config

runProgram :: ProgramConfig -> IO ()
runProgram (OutputBinaryConfig inputFile' outputFile' imageConfig' optimizationLevel') =
  toIO $ compile nullReceiver imageConfig' optimizationLevel' inputFile' outputFile'
runProgram (RunJITConfig inputFile' imageConfig' optimizationLevel') =
  toIO $ run nullReceiver imageConfig' optimizationLevel' inputFile'
runProgram (OutputGraphConfig inputFile' imageConfig') =
  toIO $ printGraphText imageConfig' inputFile'

toIO :: ExceptT PietError IO () -> IO ()
toIO = either printError return <=< runExceptT

printGraphText :: ImageConfig -> FilePath -> ExceptT PietError IO ()
printGraphText imageConfig' inputPath' =
  lift . T.putStrLn =<< graphText nullReceiver imageConfig' inputPath'
