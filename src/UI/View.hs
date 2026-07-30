{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.View where

import Brick hiding (Down)
import Brick.Keybindings (keybindingHelpWidget)
import qualified Brick.Keybindings.KeyDispatcher as KD
import Brick.Widgets.Border
import Data.Time (Day)
import Data.Zipper
import Relude
import UI.AppState
import UI.KeyEvent (KeyEvent, keyConfig)
import UI.Style (selected)
import UI.View.Markdown (drawMarkdown)

type AppEventM = EventM Resources AppState

draw :: [KD.KeyEventHandler KeyEvent AppEventM] -> AppState -> [Widget Resources]
draw handlers appState@(AppState {..}) =
  if help then [helpWidget] else [hBox $ border <$> boxes]
  where
    helpWidget = keybindingHelpWidget keyConfig handlers
    boxes = [drawSideBar appState, drawContent (current entries) markdown]

drawSideBar :: AppState -> Widget Resources
drawSideBar AppState {..} =
  hLimit 13 $ withVScrollBars OnRight $ viewport SideBar Vertical $ do
    padLeft (Pad 1) $ vBox $ do
      mconcat
        [ drawEntry <$> reverse (next entries),
          [selected $ visible $ drawEntry $ current entries],
          drawEntry <$> previous entries
        ]

drawContent :: Day -> Text -> Widget Resources
drawContent d =
  let vp = Content d
   in withVScrollBars OnRight . viewport vp Vertical . padAll 1 . drawMarkdown

drawEntry :: Day -> Widget n
drawEntry d = hBox [txt $ show d]
