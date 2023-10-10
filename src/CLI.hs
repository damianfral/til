{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI (runApp) where

import Brick (defaultMain)
import Data.Text (pack)
import Data.Version (showVersion)
import Options.Generic
import Paths_til
import Relude hiding (on)
import UI

--------------------------------------------------------------------------------

data CLI w = CLI
  { directory :: w ::: FilePath <?> "Log directory" <!> "./",
    editor :: w ::: Text <?> "Editor to open markdown files" <!> "hx"
  }
  deriving (Generic)

instance ParseRecord (CLI Wrapped)

runApp :: IO ()
runApp = do
  options <-
    unwrapRecord $
      Relude.unwords
        [ "til",
          "v" <> pack (showVersion version),
          "- a simple journal/log application"
        ]

  let appConfig = AppConfig $ directory options
  initialAppState <- loadJournalDirectory appConfig
  void $ defaultMain (app appConfig) initialAppState
  putStrLn ""
