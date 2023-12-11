{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Actions.CourseDirectory (createNewAssessmentInLocation, showErr)
import App (App, appOptions, mkApp)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.State.Strict (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), reader)
import Data.Configuration (ConfigurationPath (NewConfiguration))
import Data.Options (
  AssessmentCopyToLocation,
  ProgramCommand (CreateAssessment, GradingCLI),
  appArgs,
  currentWorkingDir,
  programCommand,
 )
import Exceptions (AppExceptionMonad)
import Parsing.Configuration (findOrCreateDefaultConfig)
import Path (Abs, Dir, Path, fromAbsDir)
import Path.IO (ensureDir, getAppUserDataDir, getCurrentDir)
import Printing (
  sgrGray,
  sgrHyperlinkFile,
  sgrPutStr,
  sgrPutStrLn,
  sgrYellow,
  startSGRScope,
 )
import StringFormatters (cleanAbsDirPathString, cleanAbsFilePathString)

showStartingDirMsg :: IO ()
showStartingDirMsg = do
  startSGRScope sgrGray $ do
    dir <- liftIO getCurrentDir
    sgrPutStrLn . unwords $ ["\nUniHs starting in:", cleanAbsDirPathString dir]

printConfigCreatedMessage :: String -> IO ()
printConfigCreatedMessage configPath = do
  startSGRScope sgrYellow $ do
    sgrPutStr "Created a new default config at \""
    sgrHyperlinkFile configPath
    sgrPutStrLn "\".\n"

  putStrLn "Please modify before re-running the application."

main :: IO ()
main = do
  startupWithConfig

getConfig :: IO (AppExceptionMonad ConfigurationPath)
getConfig = runExceptT $ do
  appDir <- liftIO $ getAppUserDataDir "uni-hs"
  liftIO $ ensureDir appDir
  liftIO $ findOrCreateDefaultConfig appDir "config.json"

startupWithConfig :: IO ()
startupWithConfig = do
  configPath <- getConfig
  showStartingDirMsg
  case configPath of
    Left err -> print err
    Right (NewConfiguration path) ->
      printConfigCreatedMessage $ cleanAbsFilePathString path
    Right cfg -> do
      void . runExceptT $ do
        app <- ExceptT $ mkApp cfg
        runReaderT runApp app

runApp :: (MonadIO m) => ReaderT App m ()
runApp = do
  command <- reader (^. appOptions . appArgs . programCommand)
  cwd <- reader (^. appOptions . currentWorkingDir)
  liftIO $ case command of
    CreateAssessment loc -> newAssessment loc cwd
    GradingCLI -> print "This feature is currently not implemented."

newAssessment :: AssessmentCopyToLocation -> Path Abs Dir -> IO ()
newAssessment loc cwd = do
  newAssess <- createNewAssessmentInLocation loc cwd
  case newAssess of
    Left ex -> showErr ex
    Right assess -> do
      let p = fromAbsDir assess
      startSGRScope sgrYellow $ do
        sgrPutStr "Assessment created in: \""
        sgrHyperlinkFile p
        sgrPutStrLn "\".\n"
