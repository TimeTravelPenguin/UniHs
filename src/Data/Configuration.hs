{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Configuration (
  -- * Type Alias
  ReplacementActionName,
  ReplacementSearchString,
  TemplateName,

  -- * Data Types

  -- ** Text Replacement Action
  TextReplacementAction (..),
  replacementActionName,
  replacementActionSearchString,
  replacementText,

  -- ** Template Configuration
  AuthorInfo (..),
  TemplateConfiguration (..),
  authorInfo,
  templateDirectoryPath,
  relativeOutputPath,
  fileContentReplacementMap,

  -- ** Configuration
  Configuration (..),
  contentReplacementActions,
  templateConfigurations,

  -- ** Other datatypes
  ConfigurationPath (NewConfiguration, ExistingConfiguration),
  configurationPath,
  isNewConfiguration,
) where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, Rel)

type ReplacementActionName = Text
type ReplacementSearchString = Text
type TemplateName = Text

-- TODO: Clean these docs

-- | A text replacement action to perform on the contents of a file,
-- as read from a configuration file.
data TextReplacementAction = TextReplacementAction
  { _replacementActionName :: ReplacementActionName
  -- ^ The unique name for the replacement action
  , _replacementActionSearchString :: ReplacementSearchString
  -- ^ The String to find and replace with a file
  , _replacementText :: Text
  -- ^ The value to substitute in place of the found search string.
  -- There are some special values that can be used based on the current directory:
  --
  -- +------------------------+-----------------------------------------------------+
  -- | _replacementText Value |                  Substituted Value                  |
  -- +========================+=====================================================+
  -- |     %%THECOURSE%%      | The course code found in the current directory.     |
  -- +------------------------+-----------------------------------------------------+
  -- |     %%THEASSESSNO%%    | The numeric value of the current assessment folder. |
  -- +------------------------+-----------------------------------------------------+
  -- |     %%THEAUTHOR%%      | The author as specified in the used template.       |
  -- |                        | If no author is given, no replacement will be made. |
  -- +------------------------+-----------------------------------------------------+
  --
  -- Any other values will be substituted as is.
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Information about authors for `TemplateConfiguration`s.
data AuthorInfo
  = -- | A single author
    SingularAuthor Text
  | -- | A non-empty collection of authors
    MultipleAuthors (NonEmpty Text)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A template to specify a directory of files to copy to a new location.
data TemplateConfiguration = TemplateConfiguration
  { _authorInfo :: Maybe AuthorInfo
  -- ^ The author of the current template
  , _templateDirectoryPath :: Path Abs Dir
  -- ^ The absolute path to the directory of files consisting of the template
  , _relativeOutputPath :: Path Rel Dir
  -- ^ The path relative to the assessment root (or current directory when using --here)
  -- where the contents of the template folder will be copied into.
  -- An empty path will copy directly into the root directory, without a new folder.
  , _fileContentReplacementMap :: Map (Path Rel File) (Seq Text)
  -- ^ A mapping of files relative to the template directory with `TextReplacementAction`s to perform.
  -- Keys are the relative file paths and values are `Seq Text` of names to `TextReplacementActions`.
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A template configuration used to copy a directory when creating a new assessment.
data Configuration = Configuration
  { _contentReplacementActions :: Seq TextReplacementAction
  -- ^ A collection of replacement actions to perform on specified file contents
  , -- TODO: Allow for copy templates to be compositions, which are lists of existing copy template names
    _templateConfigurations :: Map TemplateName TemplateConfiguration
  -- ^ A mapping of template configuration names to configurations
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ConfigurationPath
  = NewConfiguration (Path Abs File)
  | ExistingConfiguration (Path Abs File)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

configurationPath :: ConfigurationPath -> Path Abs File
configurationPath (NewConfiguration path) = path
configurationPath (ExistingConfiguration path) = path

isNewConfiguration :: ConfigurationPath -> Bool
isNewConfiguration cfg =
  case cfg of
    NewConfiguration _ -> True
    ExistingConfiguration _ -> False

makeLenses ''TextReplacementAction
makeLenses ''TemplateConfiguration
makeLenses ''Configuration

{-
instance ToJSON TextReplacementAction
instance FromJSON TextReplacementAction
instance ToJSON CopyTemplate
instance FromJSON CopyTemplate
instance ToJSON CopyConfiguration
instance FromJSON CopyConfiguration
-}

{-
instance ToJSON TextReplacementAction where
  toJSON v =
    object
      [ "ActionName" .= (v ^. replacementActionName)
      , "SearchString" .= (v ^. replacementActionSearchString)
      ]

instance FromJSON TextReplacementAction where
  parseJSON = withObject "TextReplacementAction" $ \v ->
    TextReplacementAction
      <$> v .: "ActionName"
      <*> v .: "SearchString"

instance ToJSON CopyTemplate where
  toJSON v =
    object
      [ "TemplateFolderDirectory" .= (v ^. templateFolderDirectory)
      , "RelativeOutputPath" .= (v ^. relativeOutputPath)
      , "FileContentReplacementMap" .= (v ^. fileContentReplacementMap)
      ]

instance FromJSON CopyTemplate where
  parseJSON = withObject "CopyTemplate" $ \v ->
    CopyTemplate
      <$> v .: "TemplateFolderDirectory"
      <*> v .: "RelativeOutputPath"
      <*> v .: "FileContentReplacementMap"

instance ToJSON CopyConfiguration where
  toJSON v =
    object
      [ "CopyTemplates" .= (v ^. copyTemplates)
      , "ContentReplacementActions" .= (v ^. contentReplacementActions)
      ]

instance FromJSON CopyConfiguration where
  parseJSON = withObject "CopyConfiguration" $ \v ->
    CopyConfiguration
      <$> v .: "ContentReplacementActions"
      <*> v .: "CopyTemplates"
-}
