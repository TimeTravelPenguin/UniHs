{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.CourseDirectory (
  SubjectCode,
  SubjectIdentifier,
  CourseCode (CourseCode),
  CourseDirectory (CourseDirectory),
  AssessmentsDirectory (AssessmentsDirectory),
  AssessmentItem (AssessmentItem),
  mkCourseCode,
  mkAssessmentItem,
  -- Lenses
  assessmentItemNumber,
  assessmentItemDirectory,
  assessmentsDirectoryPath,
  assessmentItems,
  courseDirectoryPath,
  courseAssessmentsDirectoryRoot,
  subjectCode,
  numericIdentifier,
) where

import Control.Lens (makeLenses)
import Data.Either.Combinators (rightToMaybe)
import Data.Sequence (Seq)
import Data.Void (Void)
import Parsing.CourseDirectory (parseAssessmentItemFolderNumber, parseCourseCode)
import Path (Abs, Dir, Path)
import StringFormatters (cleanDirNameString)
import Text.Megaparsec (ParseErrorBundle)

type SubjectCode = String

type SubjectIdentifier = Integer

data CourseCode = CourseCode
  { _subjectCode :: SubjectCode
  , _numericIdentifier :: SubjectIdentifier
  }
  deriving (Eq)

data CourseDirectory = CourseDirectory
  { _courseDirectoryPath :: Path Abs Dir
  , _courseAssessmentsDirectoryRoot :: Maybe AssessmentsDirectory
  }
  deriving (Eq, Show)

data AssessmentsDirectory = AssessmentsDirectory
  { _assessmentsDirectoryPath :: Path Abs Dir
  , _assessmentItems :: Seq AssessmentItem
  }
  deriving (Eq, Show)

data AssessmentItem = AssessmentItem
  { _assessmentItemNumber :: Integer
  , _assessmentItemDirectory :: Path Abs Dir
  }
  deriving (Eq, Show)

makeLenses ''AssessmentItem
makeLenses ''AssessmentsDirectory
makeLenses ''CourseDirectory
makeLenses ''CourseCode

-- TODO: Move these mk functions to Parsing
mkCourseCode :: String -> Maybe CourseCode
mkCourseCode =
  rightToMaybe
    . fmap (\(subj, ident) -> CourseCode subj $ read ident)
    . parseCourseCode

mkAssessmentItem :: Path Abs Dir -> Either (ParseErrorBundle String Void) AssessmentItem
mkAssessmentItem assessPath = do
  let folderName = cleanDirNameString assessPath
  num <- parseAssessmentItemFolderNumber folderName
  return $ AssessmentItem num assessPath
