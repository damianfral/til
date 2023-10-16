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
import Data.Time (Day, UTCTime (UTCTime), diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
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
    editor :: w ::: Text <?> "Editor to open markdown files" <!> "hx"
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
  options <-
    unwrapRecord $
      Relude.unwords
        [ "til",
          "v" <> pack (showVersion version),
          "- a simple journal/log application"
        ]

  let appConfig = AppConfig $ directory options
  initialAppState <- loadJournalDirectory appConfig
  chan <- newBChan 1
  asyncUpdate <- async $ forever $ updateCurrentDay chan
  void $ mkCustomMain appConfig initialAppState chan
  cancel asyncUpdate
  exitSuccess

mkCustomMain :: AppConfig -> AppState -> BChan Day -> IO AppState
mkCustomMain appConfig initialAppState chan = do
  let buildVty = do
        v <- mkVty =<< standardIOConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        pure v
  initialVty <- liftIO buildVty
  customMain initialVty buildVty (Just chan) (app appConfig) initialAppState
