{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.KeyEvent where

import qualified Brick.Keybindings.KeyConfig as KC
import qualified Brick.Keybindings.KeyEvents as KE
import qualified Graphics.Vty as V
import Relude

data KeyEvent
  = EvExit
  | EvShowHelp
  | EvRefresh
  | EvSelectDayBefore
  | EvSelectDayAfter
  | EvScrollDown
  | EvScrollUp
  | EvEdit
  deriving (Show, Eq, Ord)

myKeyEvents :: KE.KeyEvents KeyEvent
myKeyEvents =
  KE.keyEvents
    [ ("exit", EvExit),
      ("show-help", EvShowHelp),
      ("refresh", EvRefresh),
      ("select-day-before", EvSelectDayBefore),
      ("select-day-after", EvSelectDayAfter),
      ("scroll-down", EvScrollDown),
      ("scroll-up", EvScrollUp),
      ("edit", EvEdit)
    ]

defaultBindings :: [(KeyEvent, [KC.Binding])]
defaultBindings =
  [ (EvExit, [KC.bind V.KEsc, KC.bind (V.KChar 'q')]),
    (EvShowHelp, [KC.bind (V.KChar 'h')]),
    (EvRefresh, [KC.bind (V.KChar 'r')]),
    (EvSelectDayBefore, [KC.bind (V.KChar 'J'), KC.ctrl 'p']),
    (EvSelectDayAfter, [KC.bind (V.KChar 'K'), KC.ctrl 'n']),
    (EvScrollDown, [KC.bind (V.KChar 'j'), KC.bind V.KPageDown]),
    (EvScrollUp, [KC.bind (V.KChar 'k'), KC.bind V.KPageUp]),
    (EvEdit, [KC.bind (V.KChar 'e')])
  ]

keyConfig :: KC.KeyConfig KeyEvent
keyConfig = KC.newKeyConfig myKeyEvents defaultBindings []
