{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI (makeApp, customMain') where

import Brick hiding (Down)
import Brick.BChan
import Brick.Keybindings (KeyDispatcher)
import Data.Generics.Labels ()
import Data.Time (Day)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Relude hiding ((<|>))
import UI.AppConfig (AppConfig (..))
import UI.AppState
import UI.EventHandler
import UI.KeyEvent (KeyEvent (..))
import UI.Style (styleMap)
import UI.View

--------------------------------------------------------------------------------

makeApp ::
  AppConfig -> KeyDispatcher KeyEvent AppEventM -> App AppState Day Resources
makeApp appConfig keyDispatcher' = App {..}
  where
    appDraw = draw (makeKeyEventHandlers appConfig)
    appChooseCursor _ _ = Nothing
    appHandleEvent = handleEvent keyDispatcher'
    appStartEvent = pure ()
    appAttrMap = pure styleMap

customMain' ::
  AppConfig ->
  KeyDispatcher KeyEvent AppEventM ->
  AppState ->
  BChan Day ->
  IO AppState
customMain' appConfig keyDispatcher' initialAppState chan = do
  let buildVty = do
        v <- mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v
  initialVty <- liftIO buildVty
  let app = makeApp appConfig keyDispatcher'
  customMain initialVty buildVty (Just chan) app initialAppState
