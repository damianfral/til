{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI (app, AppConfig (..), AppState, loadJournalDirectory, getCurrentDay) where

import Brick
import Brick.Widgets.Border
import Control.Exception (try)
import Control.Lens
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NE
import Data.Time (Day)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Data.Time.LocalTime
import qualified Data.Time.Parsers as P
import Data.Zipper
import Graphics.Vty (Key (..))
import qualified Graphics.Vty as V
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

data AppState = AppState {entries :: Zipper Day, markdown :: Text}
  deriving (Eq, Show, Ord, Generic)

data Resources = SideBar | Content Day
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------

app :: AppConfig -> App AppState Day Resources
app appConfig = App {..}
  where
    appDraw = draw
    appChooseCursor _ _ = Nothing
    appHandleEvent = eventHandler appConfig
    appStartEvent = pure ()
    appAttrMap = pure styleMap

draw :: AppState -> [Widget Resources]
draw appState@(AppState {..}) = pure $ hBox $ border <$> boxes
  where
    boxes = [drawSideBar appState, drawContent (current entries) markdown]

drawSideBar :: AppState -> Widget Resources
drawSideBar AppState {..} =
  hLimit 13 $
    withVScrollBars OnRight $
      viewport SideBar Vertical $
        padLeft (Pad 1) $
          vBox $
            mconcat
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

eventHandler ::
  AppConfig -> BrickEvent Resources Day -> EventM Resources AppState ()
eventHandler _ (AppEvent day) = modify $ #entries . #next %~ (<> [day])
eventHandler appConfig (VtyEvent evt) = case evt of
  V.EvKey KEsc _ -> halt
  V.EvKey (KChar 'q') _ -> halt
  V.EvKey (KChar 'r') _ -> refreshCurrentFile appConfig
  V.EvKey (KChar 'J') _ -> do
    modify $ #entries %~ movePrev
    refreshCurrentFile appConfig
  V.EvKey (KChar 'K') _ -> do
    modify $ #entries %~ moveNext
    refreshCurrentFile appConfig
  V.EvKey (KChar 'j') _ -> increaseScrollContent
  V.EvKey (KChar 'k') _ -> decreaseScrollContent
  V.EvKey (KChar 'e') _ -> editContent appConfig
  _ -> pure ()
eventHandler _ _ = pure ()

refreshCurrentFile :: AppConfig -> EventM Resources AppState ()
refreshCurrentFile appConfig = do
  entries <- gets $ view #entries
  md <- readLogFile $ dayToFilePath appConfig (current entries)
  modify $ #markdown .~ md

increaseScrollContent :: EventM Resources AppState ()
increaseScrollContent = do
  day <- gets $ view $ #entries . #current
  let resource = Content day
  vScrollBy (viewportScroll resource) 1

decreaseScrollContent :: EventM Resources AppState ()
decreaseScrollContent = do
  day <- gets $ view $ #entries . #current
  let resource = Content day
  vScrollBy (viewportScroll resource) (-1)

editContent :: AppConfig -> EventM Resources AppState ()
editContent appConfig = do
  day <- gets $ view $ #entries . #current
  let file = dayToFilePath appConfig day
  suspendAndResume' $ callProcess (appConfigEditor appConfig) [file]

loadJournalDirectory :: AppConfig -> IO AppState
loadJournalDirectory appConfig@AppConfig {..} = do
  today <- getCurrentDay
  paths <- Relude.filter isMarkdownFile <$> listDirectory appConfigLogPath
  let daysFromFiles = rights $ parseDay appConfigLogPath <$> paths
  let days = case reverse $ sort daysFromFiles of
        [] -> today :| []
        mostRecentDay : rest ->
          if mostRecentDay == today
            then today :| rest
            else today :| daysFromFiles
  let zippers = NE.reverse days <&> \day -> Zipper day [] []
  let entries = sconcat zippers
  entryContent <- readLogFile $ dayToFilePath appConfig $ entries ^. #current
  pure $ AppState {entries = entries, markdown = entryContent}

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
