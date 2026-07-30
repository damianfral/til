{-# LANGUAGE DeriveGeneric #-}

module UI.AppConfig where

import Data.Generics.Labels ()
import Relude hiding ((<|>))

data AppConfig = AppConfig
  { appConfigLogPath :: FilePath,
    appConfigEditor :: FilePath
  }
  deriving (Eq, Show, Ord, Generic)
