{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI (runApp) where

import Brick (customMain)
import Brick.BChan
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.Text (pack)
import Data.Time
import Data.Version (showVersion)
import Graphics.Vty
import qualified Graphics.Vty as Vty
import Options.Generic
import Paths_til
import Relude hiding (on)
import UI

--------------------------------------------------------------------------------

data CLI w = CLI
  { directory :: w ::: FilePath <?> "Log directory" <!> "./",
    editor :: w ::: FilePath <?> "Editor to open markdown files" <!> "vi"
  }
  deriving (Generic)

instance ParseRecord (CLI Wrapped)

updateCurrentDay :: BChan Day -> IO ()
updateCurrentDay chan = do
  now@(UTCTime currentDay _) <- getCurrentTime
  let tomorrow = UTCTime (succ currentDay) 1
  let delay = diffUTCTime tomorrow now
  threadDelay $ round $ nominalDiffTimeToSeconds delay * 10 ^ (12 :: Int)
  newCurrentDay <- getCurrentDay
  writeBChan chan newCurrentDay

runApp :: IO ()
runApp = do
  options <- unwrapRecord $ unwords ["til", "v" <> pack (showVersion version)]
  let appConfig = AppConfig (directory options) (editor options)
  initialAppState <- loadJournalDirectory appConfig
  chan <- newBChan 1
  asyncUpdate <- async $ forever $ updateCurrentDay chan
  void $ customMain' appConfig initialAppState chan
  cancel asyncUpdate
  exitSuccess

customMain' :: AppConfig -> AppState -> BChan Day -> IO AppState
customMain' appConfig initialAppState chan = do
  let buildVty = do
        v <- mkVty =<< standardIOConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        pure v
  initialVty <- liftIO buildVty
  customMain initialVty buildVty (Just chan) (app appConfig) initialAppState
