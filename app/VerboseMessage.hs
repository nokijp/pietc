module VerboseMessage
  ( printVerboseMessage
  ) where

import Language.Piet
import System.IO

printVerboseMessage :: PietStep -> IO ()
printVerboseMessage = hPutStrLn stderr . stepToMessage

stepToMessage :: PietStep -> String
stepToMessage StepReadImage = "reading image..."
stepToMessage StepParse = "parsing..."
stepToMessage StepMakeAssembly = "making assembly..."
stepToMessage StepGenerateExecutable = "generating executable..."
stepToMessage StepRunJIT = "running..."
stepToMessage StepGenerateDOT = "generating DOT script..."
