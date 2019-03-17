module ErrorMessage
  ( printError
  ) where

import Language.Piet
import System.IO

printError :: PietError -> IO ()
printError = hPutStrLn stderr . errorToMessage

errorToMessage :: PietError -> String
errorToMessage (PietImageReaderError (ReadImageFileError s)) = "error: " ++ s
errorToMessage (PietImageReaderError (UnsupportedImageError s)) = "error: " ++ s
errorToMessage (PietImageReaderError CodelSizeError) = "error: the given codel size is invalid"
errorToMessage (PietParserError EmptyBlockTableError) = "error: the image has no color block"
errorToMessage (PietParserError IllegalInitialColorError) = "error: the initial codel is black"
errorToMessage (PietParserError (MissingCodelIndexError n)) = "unexpected error: codel index " ++ show n ++ " is not found"
errorToMessage (PietObjectGeneratorError (CompileError s)) = "error: " ++ s
errorToMessage (PietObjectGeneratorError (LinkError s)) = "error: " ++ s
errorToMessage (PietObjectGeneratorError (TempFileError s)) = "error: " ++ s
