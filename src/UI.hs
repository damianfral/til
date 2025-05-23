{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI (makeApp, AppConfig (..), AppState, loadJournalDirectory, getCurrentDay, customMain', makeKeyDispatcher, markdownHelp) where

import Brick hiding (Down)
import Brick.BChan
import Brick.Keybindings
import Brick.Widgets.Border
import Control.Exception (try)
import Control.Lens
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import Data.Time (Day)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Data.Time.LocalTime
import qualified Data.Time.Parsers as P
import Data.Zipper
import Graphics.Vty
import qualified Graphics.Vty as V
import qualified Graphics.Vty as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Relude
import System.Directory (listDirectory)
import System.FilePath
import qualified System.FilePath as FP
import System.Process (callProcess)
import qualified Text.Parsec as Parsec
import UI.Markdown (drawMarkdown)
import UI.Style (selected, styleMap)

data AppConfig = AppConfig
  { appConfigLogPath :: FilePath,
    appConfigEditor :: FilePath
  }
  deriving (Eq, Show, Ord, Generic)

data AppState = AppState {entries :: Zipper Day, markdown :: Text, help :: Bool}
  deriving (Eq, Show, Ord, Generic)

data Resources = SideBar | Content Day
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------

makeApp ::
  AppConfig -> KeyDispatcher Action AppEventM -> App AppState Day Resources
makeApp appConfig keyDispatcher' = App {..}
  where
    appDraw = draw appConfig
    appChooseCursor _ _ = Nothing
    appHandleEvent = eventHandler keyDispatcher'
    appStartEvent = pure ()
    appAttrMap = pure styleMap

draw :: AppConfig -> AppState -> [Widget Resources]
draw appConfig appState@(AppState {..}) =
  if help
    then [keybindingHelpWidget keyConfig (makeKeyEventHandlers appConfig)]
    else pure $ hBox $ border <$> boxes
  where
    boxes = [drawSideBar appState, drawContent (current entries) markdown]

drawSideBar :: AppState -> Widget Resources
drawSideBar AppState {..} =
  hLimit 13
    $ withVScrollBars OnRight
    $ viewport SideBar Vertical
    $ padLeft (Pad 1)
    $ vBox
    $ mconcat
      [ drawEntry <$> reverse (next entries),
        [selected $ visible $ drawEntry $ current entries],
        drawEntry <$> previous entries
      ]

drawContent :: Day -> Text -> Widget Resources
drawContent day =
  withVScrollBars OnRight
    . viewport (Content day) Vertical
    . padAll 1
    . drawMarkdown

drawEntry :: Day -> Widget n
drawEntry day = hBox [txt $ show day]

--------------------------------------------------------------------------------

data Action
  = Exit
  | ShowHelp
  | Refresh
  | SelectDayBefore
  | SelectDayAfter
  | ScrollDown
  | ScrollUp
  | Edit
  deriving (Show, Eq, Ord)

keyConfig :: KeyConfig Action
keyConfig = newKeyConfig keyEventsMap [] []
  where
    keyEventsMap =
      keyEvents
        [ ("exit", Exit),
          ("show-help", ShowHelp),
          ("refresh", Refresh),
          ("select-day-before", SelectDayBefore),
          ("select-day-after", SelectDayAfter),
          ("select-day-after", SelectDayAfter),
          ("scroll-down", ScrollDown),
          ("scroll-up", ScrollUp)
        ]

type AppEventM = EventM Resources AppState

makeKeyEventHandlers :: AppConfig -> [KeyEventHandler k AppEventM]
makeKeyEventHandlers appConfig =
  [ onKey KEsc "exit" halt,
    onKey (KChar 'q') "exit" halt,
    onKey (KChar 'h') "help" $ modify $ #help %~ not,
    onKey (KChar 'r') "refresh current entry" $ refreshCurrentFile appConfig,
    onKey (KChar 'J') "select day before" $ do
      modify $ #entries %~ movePrev
      refreshCurrentFile appConfig,
    onKey (KChar 'K') "select day after" $ do
      modify $ #entries %~ moveNext
      refreshCurrentFile appConfig,
    onKey (KChar 'j') "increase scroll" increaseScrollContent,
    onKey (KChar 'k') "decrease scroll" decreaseScrollContent,
    onKey (KChar 'e') "edit entry" $ do
      editContent appConfig >> refreshCurrentFile appConfig
  ]

markdownHelp :: AppConfig -> Text
markdownHelp appConfig =
  keybindingMarkdownTable keyConfig [("", makeKeyEventHandlers appConfig)]

--------------------------------------------------------------------------------

eventHandler ::
  KeyDispatcher k AppEventM -> BrickEvent Resources Day -> AppEventM ()
eventHandler _ (AppEvent day) = modify $ #entries . #next %~ (<> [day])
eventHandler keyDispatcher' (VtyEvent evt) = void $ case evt of
  V.EvKey kchar mods -> void $ handleKey keyDispatcher' kchar mods
  _ -> pure ()
eventHandler _ _ = pure ()

refreshCurrentFile :: AppConfig -> AppEventM ()
refreshCurrentFile appConfig = do
  entries <- gets $ view #entries
  md <- readLogFile $ dayToFilePath appConfig (current entries)
  modify $ #markdown .~ md

increaseScrollContent :: AppEventM ()
increaseScrollContent = do
  day <- gets $ view $ #entries . #current
  let resource = Content day
  vScrollBy (viewportScroll resource) 1

decreaseScrollContent :: AppEventM ()
decreaseScrollContent = do
  day <- gets $ view $ #entries . #current
  let resource = Content day
  vScrollBy (viewportScroll resource) (-1)

editContent :: AppConfig -> AppEventM ()
editContent appConfig = do
  day <- gets $ view $ #entries . #current
  let file = dayToFilePath appConfig day
  suspendAndResume' $ callProcess (appConfigEditor appConfig) [file]

loadJournalDirectory :: AppConfig -> IO AppState
loadJournalDirectory appConfig@AppConfig {..} = do
  today <- getCurrentDay
  paths <- Relude.filter isMarkdownFile <$> listDirectory appConfigLogPath
  let daysFromFiles = rights $ parseDay appConfigLogPath <$> paths
  let previousDays =
        case sortBy (comparing Down) daysFromFiles of
          [] -> []
          mostRecentDay : rest ->
            if mostRecentDay == today then rest else mostRecentDay : rest
  let entries = Zipper today previousDays []
  entryContent <- readLogFile $ dayToFilePath appConfig $ entries ^. #current
  pure $ AppState {entries = entries, markdown = entryContent, help = False}

parseDay :: Parsec.SourceName -> FilePath -> Either Parsec.ParseError Day
parseDay = Parsec.runParser P.day ()

isMarkdownFile :: FilePath -> Bool
isMarkdownFile file = takeExtension file == ".md"

dayToFilePath :: AppConfig -> Day -> FilePath
dayToFilePath AppConfig {..} day =
  appConfigLogPath </> formatShow iso8601Format day FP.<.> "md"

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime

readLogFile :: (MonadIO m) => FilePath -> m Text
readLogFile file = do
  eContent <- liftIO $ try $ BS.readFile file
  case eContent of
    Left (SomeException _) -> pure ""
    Right c -> pure $ decodeUtf8With lenientDecode c

customMain' ::
  AppConfig -> KeyDispatcher Action AppEventM -> AppState -> BChan Day -> IO AppState
customMain' appConfig keyDispatcher' initialAppState chan = do
  let buildVty = do
        v <- mkVty defaultConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        pure v
  initialVty <- liftIO buildVty
  let app = makeApp appConfig keyDispatcher'
  customMain initialVty buildVty (Just chan) app initialAppState

makeKeyDispatcher ::
  AppConfig -> IO (KeyDispatcher Action (EventM Resources AppState))
makeKeyDispatcher appConfig = do
  case keyDispatcher keyConfig (makeKeyEventHandlers appConfig) of
    Right v -> pure v
    Left _ -> putStrLn "Error creating KeyDispatcher" >> exitFailure
