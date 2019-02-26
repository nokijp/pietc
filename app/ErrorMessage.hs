module ErrorMessage
  ( errorToMessage
  ) where

import Language.Piet

errorToMessage :: PietError -> String
errorToMessage (PietImageReaderError (FileReadError s)) = "error: " ++ s
errorToMessage (PietImageReaderError (ImageTypeError s)) = "error: " ++ s
errorToMessage (PietImageReaderError CodelSizeError) = "error: the given codel size is invalid"
errorToMessage (PietParserError EmptyBlockTableError) = "error: the image has no color block"
errorToMessage (PietParserError (IllegalInitialColorError c)) = "error: the initial codel is an invalid color " ++ show c
errorToMessage (PietParserError (MissingCodelIndexError n)) = "unexpected error: codel index " ++ show n ++ " is not found"
errorToMessage (PietParserError (IllegalCoordinateError x y)) = "unexpected error: (" ++ show x ++ ", " ++ show y ++ ") is an invalid image coordinate"
errorToMessage (PietObjectGeneratorError (CompileError s)) = "error: " ++ s
errorToMessage (PietObjectGeneratorError (LinkError s)) = "error: " ++ s
errorToMessage (PietObjectGeneratorError (TempFileError s)) = "error: " ++ s
