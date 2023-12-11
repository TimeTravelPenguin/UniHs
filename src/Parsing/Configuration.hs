{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsing.Configuration (
  ifNewConfiguration,
  createDemoTemplateFileIfNotExist,
  createDefaultConfigFile,
  findOrCreateDefaultConfig,
  parseApplicationConfig,
) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (unless)
import Data.Aeson (decodeFileStrict)
import Data.Configuration (
  AuthorInfo (SingularAuthor),
  Configuration (..),
  ConfigurationPath (..),
  TemplateConfiguration (TemplateConfiguration, _authorInfo, _fileContentReplacementMap, _relativeOutputPath, _templateDirectoryPath),
  TextReplacementAction (TextReplacementAction),
  configurationPath,
  fileContentReplacementMap,
  replacementActionName,
  templateDirectoryPath,
 )
import Data.Either.Extra (maybeToEither)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Arg (Arg))
import Data.Sequence qualified as Seq
import Exceptions (AppException (InvalidConfiguration), AppExceptionMonad)
import Helpers.IO (encodeFilePretty)
import Path (Abs, Dir, File, Path, fromAbsFile, parent, reldir, relfile)
import Path.IO (doesFileExist, ensureDir, resolveDir, resolveFile)

ifNewConfiguration :: ConfigurationPath -> (Path Abs File -> t) -> (Path Abs File -> t) -> t
ifNewConfiguration cfg p q =
  case cfg of
    NewConfiguration config -> p config
    ExistingConfiguration config -> q config

-- Creates demo file for the default config. It will make all parents in the filepath if they do not exist.
createDemoTemplateFile :: Path Abs File -> IO ()
createDemoTemplateFile filePath = do
  ensureDir (parent filePath)
  writeFile
    (fromAbsFile filePath)
    "\\documentclass[11pt]{article}\n\
    \\n\\title{%%COURSE%% Assessment %%ASSESSNO%%}\n\
    \\\date{\\today}\n\
    \\\author{%%NAME%%}\n\
    \\n\\begin{document}\n\
    \    \\maketitle\n\
    \    Hello, there! This is just a simple \\LaTeX template!\n\
    \\\end{document}"

createDemoTemplateFileIfNotExist :: Path Abs File -> IO ()
createDemoTemplateFileIfNotExist filePath = do
  fileExists <- doesFileExist filePath
  unless fileExists $ createDemoTemplateFile filePath

createDefaultConfigFile :: Path Abs Dir -> IO (Path Abs File)
createDefaultConfigFile configPath = do
  -- Make a new template directory in config dir
  assessmentTemplatePath <- resolveDir configPath "DemoTemplates"

  -- Create demo template sub-folders
  mathTemplateDir <- resolveDir assessmentTemplatePath "MathTemplate"
  compSciTemplateDir <- resolveDir assessmentTemplatePath "CompSciTemplate"

  -- Create demo assessment template files in each sub-dir
  mathTemplate <- resolveFile mathTemplateDir "main.tex"
  compSciTemplate <- resolveFile compSciTemplateDir "main.tex"

  -- Parent directories will be created along with files
  createDemoTemplateFileIfNotExist mathTemplate
  createDemoTemplateFileIfNotExist compSciTemplate

  let replacementActions =
        [ TextReplacementAction "CourseCode" "%%COURSE%%" "%%THECOURSE%%"
        , TextReplacementAction "AssessmentNumber" "%%ASSESSNO%%" "%%THEASSESSNO%%"
        , TextReplacementAction "AuthorName" "%%NAME%%" "%%THEAUTHOR%%"
        ]
      mkActionsMap path =
        Map.fromArgSet
          [ Arg path $
              (^. replacementActionName)
                <$> Seq.fromList replacementActions
          ]
      templateConfig =
        TemplateConfiguration
          { _authorInfo = Just $ SingularAuthor "Your name"
          , _templateDirectoryPath = mathTemplateDir
          , _relativeOutputPath = [reldir|tex/|]
          , _fileContentReplacementMap = mkActionsMap [relfile|tex/file.tex|]
          }
      config =
        Configuration
          { _contentReplacementActions = Seq.fromList replacementActions
          , _templateConfigurations =
              Map.fromArgSet
                [ Arg "MathTemplate" templateConfig
                , Arg "CompSciTemplate" $
                    templateConfig
                      & (templateDirectoryPath .~ compSciTemplateDir)
                        . (fileContentReplacementMap .~ mkActionsMap [relfile|tex/files/the_file.tex|])
                ]
          }

  configFile <- resolveFile configPath "config.json"
  encodeFilePretty (fromAbsFile configFile) config
  return configFile

-- | Creates a default "config.json" in the provided directory, if one is not found.
--   TODO: MOVE
findOrCreateDefaultConfig :: Path Abs Dir -> FilePath -> IO ConfigurationPath
findOrCreateDefaultConfig dir cfgName = do
  configFile <- resolveFile dir cfgName
  configExists <- doesFileExist configFile
  if not configExists
    then NewConfiguration <$> createDefaultConfigFile dir
    else return $ ExistingConfiguration configFile

parseApplicationConfig :: ConfigurationPath -> IO (AppExceptionMonad Configuration)
parseApplicationConfig configPath =
  maybeToEither (InvalidConfiguration $ configurationPath configPath)
    <$> case configPath of
      NewConfiguration path -> decode path
      ExistingConfiguration path -> decode path
 where
  decode = decodeFileStrict . fromAbsFile
