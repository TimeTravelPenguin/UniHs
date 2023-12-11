{-# LANGUAGE RankNTypes #-}

module Commands (ConfigurationReader, runProgram) where

import Actions.CourseDirectory (createNewAssessmentInLocation, showErr)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT)
import Data.Configuration (Configuration)
import Data.Options (
  AppArgs (AppArgs),
  AssessmentCopyToLocation,
  ProgramCommand (CreateAssessment, GradingCLI),
  ProgramOptions (ProgramOptions),
  getProgramOptions,
 )
import Path (Abs, Dir, Path)
import Printing (sgrHyperlinkFile, sgrPutStr, sgrPutStrLn, sgrYellow, startSGRScope)
import StringFormatters (cleanAbsDirPathString)

type ConfigurationReader a = forall m. (MonadIO m) => ReaderT Configuration m a

-- | TODO: Need to implement configuration logics
runProgram :: ConfigurationReader ()
runProgram = do
  (ProgramOptions (AppArgs command) cwd) <- liftIO getProgramOptions
  liftIO $ case command of
    CreateAssessment loc -> newAssessmentInLocation loc cwd
    GradingCLI -> print "This feature is currently not implemented."

newAssessmentInLocation :: AssessmentCopyToLocation -> Path Abs Dir -> IO ()
newAssessmentInLocation loc dir = do
  newAssess <- createNewAssessmentInLocation loc dir
  case newAssess of
    Left ex -> showErr ex
    Right assess -> do
      let assessStr = cleanAbsDirPathString assess
      startSGRScope sgrYellow $ do
        sgrPutStr "Assessment created in: \""
        sgrHyperlinkFile assessStr
        sgrPutStrLn "\".\n"
