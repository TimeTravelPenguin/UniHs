module Exceptions (
  -- * Application exception

  -- | The monad in which application exceptions are thrown
  AppExceptionMonad,
  AppExceptionMonadT,

  -- ** Application exceptions
  AppException (
    -- \| The application directory could not be determined
    UndeterminedAppDirectory,
    -- \| The configuration file is invalid
    InvalidConfiguration,
    -- \| The file does not exist
    FileDoesNotExist
  ),
) where

import Control.Monad.Except (ExceptT)
import Path (Abs, File, Path)

type AppExceptionMonad = Either AppException
type AppExceptionMonadT = ExceptT AppException

data AppException
  = UndeterminedAppDirectory
  | InvalidConfiguration (Path Abs File)
  | FileDoesNotExist (Path Abs File)
  deriving (Show)
