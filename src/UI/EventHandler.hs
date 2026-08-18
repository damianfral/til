{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.EventHandler where

import Brick hiding (Down)
import Brick.Keybindings (KeyDispatcher)
import qualified Brick.Keybindings.KeyDispatcher as KD
import Control.Lens
import Data.Generics.Labels ()
import Data.Time (Day)
import Data.Zipper
import qualified Graphics.Vty as V
import Relude
import System.Process (callProcess)
import UI.AppConfig (AppConfig (..))
import UI.AppState
import UI.KeyEvent (KeyEvent (..), keyConfig)
import UI.View

makeKeyDispatcher :: AppConfig -> IO (KeyDispatcher KeyEvent AppEventM)
makeKeyDispatcher appConfig =
  case KD.keyDispatcher keyConfig (makeKeyEventHandlers appConfig) of
    Right v -> pure v
    Left _ -> putStrLn "Error creating KeyDispatcher" >> exitFailure

makeKeyEventHandlers :: AppConfig -> [KD.KeyEventHandler KeyEvent AppEventM]
makeKeyEventHandlers appConfig =
  [ KD.onEvent EvExit "Exit the application" halt,
    KD.onEvent EvShowHelp "Toggle help overlay" $ modify $ #help %~ not,
    KD.onEvent EvRefresh "Refresh current entry" $ refreshCurrentFile appConfig,
    KD.onEvent EvSelectDayBefore "Select day before" $ do
      modify $ #entries %~ movePrev
      refreshCurrentFile appConfig,
    KD.onEvent EvSelectDayAfter "Select day after" $ do
      modify $ #entries %~ moveNext
      refreshCurrentFile appConfig,
    KD.onEvent EvScrollDown "Scroll down" increaseScrollContent,
    KD.onEvent EvScrollUp "Scroll up" decreaseScrollContent,
    KD.onEvent EvEdit "Edit entry" $ do
      editContent appConfig >> refreshCurrentFile appConfig
  ]

handleEvent ::
  KeyDispatcher KeyEvent AppEventM -> BrickEvent Resources Day -> AppEventM ()
handleEvent _ (AppEvent d) = modify $ #entries . #next %~ (<> [d])
handleEvent keyDispatcher' (VtyEvent evt) = void $ case evt of
  V.EvKey kchar mods -> void $ KD.handleKey keyDispatcher' kchar mods
  _ -> pure ()
handleEvent _ (MouseDown vp direction _mods _location) = case direction of
  V.BScrollUp -> vScrollBy (viewportScroll vp) (-1)
  V.BScrollDown -> vScrollBy (viewportScroll vp) 1
  _ -> pure ()
handleEvent _ (MouseUp {}) = pure ()

refreshCurrentFile :: AppConfig -> AppEventM ()
refreshCurrentFile AppConfig {..} = do
  entries <- gets $ view #entries
  md <- readLogFile $ dayToFilePath appConfigLogPath (current entries)
  modify $ #markdown .~ md

increaseScrollContent :: AppEventM ()
increaseScrollContent = do
  d <- gets $ view $ #entries . #current
  let resource = Content d
  vScrollBy (viewportScroll resource) 1

decreaseScrollContent :: AppEventM ()
decreaseScrollContent = do
  d <- gets $ view $ #entries . #current
  let resource = Content d
  vScrollBy (viewportScroll resource) (-1)

editContent :: AppConfig -> AppEventM ()
editContent AppConfig {..} = do
  d <- gets $ view $ #entries . #current
  let file = dayToFilePath appConfigLogPath d
  suspendAndResume' $ callProcess appConfigEditor [file]
