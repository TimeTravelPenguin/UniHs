{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsing.CourseDirectory (
  Parser,
  pYear,
  isYear,
  pCourseCode,
  parseCourseCode,
  isCourseCode,
  pAssessmentItemFolderNumber,
  parseAssessmentItemFolderNumber,
  isAssessmentItemFolder,
)
where

import Control.Monad (void)
import Data.Either (isRight)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (eof),
  ParseErrorBundle,
  Parsec,
  count,
  parse,
  some,
  (<?>),
 )
import Text.Megaparsec.Char (digitChar, spaceChar, string, upperChar)

type Parser = Parsec Void String

pYear :: Parser String
pYear = count 4 digitChar

isYear :: String -> Bool
isYear = isRight . parse (pYear <* eof) ""

pCourseCode :: Parser (String, String)
pCourseCode = do
  courseCode <- count 4 upperChar <?> "Course Code"
  courseNum <- count 4 digitChar <?> "Course Number"
  return (courseCode, read courseNum)

parseCourseCode :: String -> Either (ParseErrorBundle String Void) (String, String)
parseCourseCode = parse (pCourseCode <* eof) ""

isCourseCode :: String -> Bool
isCourseCode = isRight . parseCourseCode

pAssessmentItemFolderNumber :: Parser Integer
pAssessmentItemFolderNumber = do
  void (string "Assessment") <?> "Assessment Item Base Name"
  void (some spaceChar) <?> "Assessment Item Space"
  folderNumber <- some digitChar <?> "Assessment Item Number"
  return $ read folderNumber

parseAssessmentItemFolderNumber :: String -> Either (ParseErrorBundle String Void) Integer
parseAssessmentItemFolderNumber = parse (pAssessmentItemFolderNumber <* eof) ""

isAssessmentItemFolder :: String -> Bool
isAssessmentItemFolder = isRight . parseAssessmentItemFolderNumber
