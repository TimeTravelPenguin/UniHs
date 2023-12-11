{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module App (
  App,
  appOptions,
  appConfig,
  mkApp,
) where

import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Configuration (Configuration, ConfigurationPath)
import Data.Options (ProgramOptions, getProgramOptions)
import Exceptions (AppExceptionMonad)
import Parsing.Configuration (parseApplicationConfig)

data App = App
  { _appOptions :: ProgramOptions
  , _appConfig :: Configuration
  }

makeLenses ''App

mkApp :: (MonadIO m) => ConfigurationPath -> m (AppExceptionMonad App)
mkApp configPath =
  runExceptT $ do
    config <- ExceptT . liftIO $ parseApplicationConfig configPath
    options <- liftIO getProgramOptions
    return $ App options config
