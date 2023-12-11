{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Data.Options

Module contains functionality for parsing command line arguments
-}
module Data.Options (
  -- * Types

  -- | These types encapsulates the provided command line arguments.
  ProgramCommand (..),
  AssessmentCopyToLocation (..),
  AppArgs (..),
  programCommand,
  ProgramOptions (..),
  appArgs,
  currentWorkingDir,

  -- * Parser functions
  argParser,
  parseProgramCommand,
  parseCommandGradingCLI,
  parseFlagAssessmentCopyToLocation,
  parseCommandNewAssessment,

  -- * Option Parsing
  getProgramOptions,
) where

import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  command,
  execParser,
  flag,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  progDesc,
  subparser,
  (<**>),
 )

import Control.Lens (makeLenses)
import Path (Abs, Dir, Path)
import Path.IO (AnyPath (makeAbsolute), getCurrentDir)

-- | Datatype for which command the program will execute.
data ProgramCommand
  = -- | Create a new assessment directory
    CreateAssessment
      AssessmentCopyToLocation
      -- ^ Whether or not to copy an assessment template here, or into a new assessment directory (via *--here* flag)
  | -- | Start the CLI to add, edit, or use hypothetical grades for various features
    GradingCLI
  deriving (Show)

-- | Data type for @--here@ flag. See `parseFlagAssessmentCopyToLocation`.
data AssessmentCopyToLocation = AssessmentsRootDir | CurrentWorkingDirectory deriving (Show)

-- | Data type containing data for parsed command line arguments.
newtype AppArgs = AppArgs
  { _programCommand :: ProgramCommand
  }
  deriving (Show)

-- | Options used at runtime containing parsed AppArgs
data ProgramOptions = ProgramOptions
  { _appArgs :: AppArgs
  , _currentWorkingDir :: Path Abs Dir
  }
  deriving (Show)

makeLenses ''AppArgs
makeLenses ''ProgramOptions

-- | Parses @assess@ command for `CreateAssessment`.
parseCommandNewAssessment :: Mod CommandFields ProgramCommand
parseCommandNewAssessment =
  command
    "assess"
    ( info
        (CreateAssessment <$> parseFlagAssessmentCopyToLocation <**> helper)
        ( progDesc "Create a new assessment folder in the current course (parent) directory, unless using --here"
        )
    )

-- | Parses @--here@ flag for `AssessmentCopyToLocation`. Using the flag results in `Here`. Otherwise, `NewCourseAssessment`.
parseFlagAssessmentCopyToLocation :: Parser AssessmentCopyToLocation
parseFlagAssessmentCopyToLocation =
  flag
    AssessmentsRootDir
    CurrentWorkingDirectory
    ( long "here"
        <> help "Create a new assessment folder in the current course (parent) directory"
    )

-- | Parses @grades@ command for `GradingCLI`.
parseCommandGradingCLI :: Mod CommandFields ProgramCommand
parseCommandGradingCLI =
  command
    "grades"
    ( info
        (pure GradingCLI <**> helper)
        ( progDesc "Start the CLI to add, edit, or use hypothetical grades for various features"
        )
    )

-- | Parses a `ProgramCommand`.
parseProgramCommand :: Parser ProgramCommand
parseProgramCommand = subparser (parseCommandNewAssessment <> parseCommandGradingCLI)

-- | Application argument parser used to parse `AppArgs`.
argParser :: Parser AppArgs
argParser = AppArgs <$> parseProgramCommand

-- | Contains application information.
appInfo :: ParserInfo AppArgs
appInfo =
  info
    (argParser <**> helper)
    ( fullDesc
        <> progDesc "A tool for performing simple university related tasks."
        <> header "UniHs.exe is made by Phillip Smith"
    )

-- | Used to create a `ProgramOptions` by parsing `AppArgs` from the commandline.
getProgramOptions :: IO ProgramOptions
getProgramOptions = do
  cwd <- getCurrentDir >>= makeAbsolute
  parser <- execParser appInfo
  return $ ProgramOptions parser cwd
