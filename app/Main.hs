module Main where

import Control.Monad
import Control.Monad.Except
import ErrorMessage
import Language.Piet
import OptParser

main :: IO ()
main = do
  config <- parseArgs
  runProgram config

runProgram :: ProgramConfig -> IO ()
runProgram (ProgramConfig (OutputBinary outputFile') inputFile' imageConfig' optimizationLevel') =
  toIO $ compile imageConfig' optimizationLevel' inputFile' outputFile'
runProgram (ProgramConfig RunJIT                     inputFile' imageConfig' optimizationLevel') =
  toIO $ run imageConfig' optimizationLevel' inputFile'

toIO :: ExceptT PietError IO () -> IO ()
toIO = either (putStrLn . errorToMessage) return <=< runExceptT
