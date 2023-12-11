{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions.CourseDirectory (
  CourseDirectoryError (..),
  getCourseDirectoryInPath,
  ensureAssessmentsDirectory,
  createNewAssessmentInLocation,
  showErr,
) where

import Control.Lens ((&), (.~), (^.), (^?), _Just)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe, maybeToExceptT)
import Data.CourseDirectory (
  AssessmentItem (..),
  AssessmentsDirectory (..),
  CourseDirectory (..),
  assessmentItemNumber,
  assessmentsDirectoryPath,
  courseAssessmentsDirectoryRoot,
  courseDirectoryPath,
  mkAssessmentItem,
 )
import Data.Either (isRight)
import Data.List (find)
import Data.Options (AssessmentCopyToLocation (..))
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as Seq
import Data.Void (Void)
import Helpers.Sequence (rights)
import Parsing.CourseDirectory (isAssessmentItemFolder, isCourseCode, isYear)
import Path (Abs, Dir, Path, Rel, parent, reldir, (</>))
import Path.IO (ensureDir, listDir, resolveDir)
import Printing (sgrHyperlinkFile, sgrPutStr, sgrPutStrLn, sgrRed, startSGRScope)
import Safe.Foldable (maximumDef)
import StringFormatters (cleanDirNameString)
import Text.Megaparsec (MonadParsec (eof), Parsec, parse)
import Text.Megaparsec.Char (string)

type Parser = Parsec Void FilePath

-- FIXME: REMOVE THIS
data CourseDirectoryError
  = CourseDirNotFound (Path Abs Dir)
  | CourseAssessmentsDirNotFound (Path Abs Dir)
  | FailedToCreateDirectory (Path Abs Dir) (Path Rel Dir)
  | OtherException String

showErr :: CourseDirectoryError -> IO ()
showErr (CourseDirNotFound path) =
  startSGRScope sgrRed $ do
    sgrPutStr "Error: CourseDirNotFound. "
    sgrPutStr "Could not find a course directory in the path "
    sgrHyperlinkFile (show path)
    sgrPutStrLn "."
showErr (CourseAssessmentsDirNotFound path) =
  startSGRScope sgrRed $ do
    sgrPutStr "Error: CourseAssessmentsDirNotFound. "
    sgrPutStr "Could not find an Assessments directory at the path"
    sgrHyperlinkFile (show path)
    sgrPutStrLn "."
showErr (FailedToCreateDirectory path rel) =
  startSGRScope sgrRed $ do
    sgrPutStr "Error: FailedToCreateDirectory."
    sgrPutStr "Could not create the directory"
    sgrHyperlinkFile (show $ path </> rel)
    sgrPutStrLn "."
showErr (OtherException msg) =
  startSGRScope sgrRed $ do
    sgrPutStr "Error: OtherException."
    sgrPutStrLn msg

-- TODO: Move this. This doesn't feel like an appropriate location.
findCourseDirInPath :: Path Abs Dir -> Maybe (Path Abs Dir)
findCourseDirInPath currentDir
  | parentIsYearDir && isCourseDir = Just currentDir
  | currentDir == currentParent = Nothing
  | otherwise = findCourseDirInPath currentParent
 where
  currentParent = parent currentDir
  parentIsYearDir = isYear . cleanDirNameString $ currentParent
  currentDirName = cleanDirNameString currentDir
  isCourseDir = isCourseCode currentDirName

getAssessmentsRootInDirectory :: (MonadIO m) => Path Abs Dir -> MaybeT m (Path Abs Dir)
getAssessmentsRootInDirectory dir = do
  (folders, _) <- listDir dir
  let folderNames = map cleanDirNameString folders
      parser = parse (string "Assessments" <* eof :: Parser FilePath) ""
      selector = find (isRight . parser)
  selectedDir <- hoistMaybe $ selector folderNames
  liftIO $ resolveDir dir selectedDir

getCourseAssessmentItemsPaths :: (MonadIO m) => Path Abs Dir -> m (Seq (Path Abs Dir))
getCourseAssessmentItemsPaths dir = do
  (folders, _) <- listDir dir
  let isValidFolder = isAssessmentItemFolder . cleanDirNameString
      validFolders = filter isValidFolder folders
  return $ Seq.fromList validFolders

getAssessmentItems :: (MonadIO m) => Path Abs Dir -> m (Seq AssessmentItem)
getAssessmentItems assessDirPath =
  rights
    . fmap mkAssessmentItem
    <$> getCourseAssessmentItemsPaths assessDirPath

getAssessmentsDirectory :: (MonadIO m) => Path Abs Dir -> MaybeT m AssessmentsDirectory
getAssessmentsDirectory courseDirPath = do
  assessmentDirPath <- getAssessmentsRootInDirectory courseDirPath
  assessItems <- getAssessmentItems assessmentDirPath
  return $ AssessmentsDirectory assessmentDirPath assessItems

getCourseDirectoryInPath :: (MonadIO m) => Path Abs Dir -> ExceptT CourseDirectoryError m CourseDirectory
getCourseDirectoryInPath path = do
  courseDir <- maybeToExceptT (CourseDirNotFound path) . hoistMaybe $ findCourseDirInPath path
  assess <- runMaybeT (getAssessmentsDirectory courseDir)
  return $ CourseDirectory courseDir assess

ensureAssessmentsDirectory :: (MonadIO m) => CourseDirectory -> m CourseDirectory
ensureAssessmentsDirectory dir = do
  let courseAssDir = dir ^. courseAssessmentsDirectoryRoot
  assessRoot <- case courseAssDir of
    Just assessDir -> return $ assessDir ^. assessmentsDirectoryPath
    Nothing -> liftIO $ resolveDir (dir ^. courseDirectoryPath) "Assessments"
  ensureDir assessRoot
  assessments <- runMaybeT . getAssessmentsDirectory $ dir ^. courseDirectoryPath
  return $ dir & (courseAssessmentsDirectoryRoot .~ assessments)

-- | Return the next assessment index greater than zero.
nextAssessmentIdx :: Seq AssessmentItem -> Integer
nextAssessmentIdx = (+) 1 . maximumDef 0 . (0 <|) . fmap (^. assessmentItemNumber)

createNewAssessmentAt :: (MonadIO m) => Path Abs Dir -> m (Path Abs Dir)
createNewAssessmentAt dir = do
  nextIdx <- nextAssessmentIdx <$> getAssessmentItems dir
  newAssesDir <- resolveDir dir $ unwords ["Assessment", show nextIdx]
  ensureDir newAssesDir
  return newAssesDir

ensureCourseDirectoryInPath :: (MonadIO m) => Path Abs Dir -> ExceptT CourseDirectoryError m CourseDirectory
ensureCourseDirectoryInPath dir = do
  courseDir <- getCourseDirectoryInPath dir
  ensureAssessmentsDirectory courseDir

-- createNewAssessmentInAssessRootOf :: MonadIO m => Path Abs Dir -> m (Either CourseDirectoryError (Path Abs Dir))
createNewAssessmentInAssessRootOf dir = do
  courseDir <- ensureCourseDirectoryInPath dir
  let assesPath = courseDir ^. courseAssessmentsDirectoryRoot ^? _Just . assessmentsDirectoryPath
  case assesPath of
    Nothing -> liftEither . Left $ FailedToCreateDirectory (courseDir ^. courseDirectoryPath) [reldir|Assessments|]
    Just path -> withExceptT OtherException $ createNewAssessmentAt path

createNewAssessmentInLocation :: (MonadIO m) => AssessmentCopyToLocation -> Path Abs Dir -> m (Either CourseDirectoryError (Path Abs Dir))
createNewAssessmentInLocation loc dir = runExceptT $ do
  case loc of
    AssessmentsRootDir -> createNewAssessmentInAssessRootOf dir
    CurrentWorkingDirectory -> withExceptT OtherException $ createNewAssessmentAt dir
