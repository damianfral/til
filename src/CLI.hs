{-# LANGUAGE NoImplicitPrelude #-}

module CLI (runApp) where

import Brick (defaultMain)
import Relude hiding (on)
import UI

--------------------------------------------------------------------------------

runApp :: IO ()
runApp = do
  initialAppState <- loadJournalDirectory myAppConfig
  void $ defaultMain (app myAppConfig) initialAppState
  putStrLn ""
