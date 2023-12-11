module Helpers.IO (getApplicationDirectory, encodeFilePretty) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), hoistMaybe, maybeToExceptT)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Exceptions (AppException (UndeterminedAppDirectory), AppExceptionMonad)
import Path (Abs, Dir, Path, parseAbsDir)
import System.Environment (executablePath)

getApplicationDirectory :: IO (AppExceptionMonad (Path Abs Dir))
getApplicationDirectory = do
  runExceptT $ do
    appDir <- maybeToExceptT UndeterminedAppDirectory $ MaybeT =<< hoistMaybe executablePath
    parseAbsDir appDir

encodeFilePretty :: (ToJSON a) => FilePath -> a -> IO ()
encodeFilePretty filePath obj = do
  let encoded = encodePretty obj
  BL.writeFile filePath encoded
