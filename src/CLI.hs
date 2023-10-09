{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI (runApp) where

import Brick
import qualified Brick.AttrMap ()
import Brick.Widgets.Border
import Control.Exception (try)
import Control.Lens
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NE
import Data.Text hiding (elem, reverse, take)
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.Time.Parsers as P
import Graphics.Vty (Key (..))
import qualified Graphics.Vty as V
import Log
import Relude hiding (on)
import System.Directory
import System.FilePath (takeExtension, (</>))
import qualified System.FilePath as FP
import System.Process
import qualified Text.Parsec as Parsec
import UI.Markdown (drawMarkdown)
import UI.Style

--------------------------------------------------------------------------------

newtype AppConfig = AppConfig {appConfigLogPath :: FilePath}
  deriving (Eq, Show, Ord, Generic)

data AppState = AppState
  {entries :: Zipper Day, markdown :: Text}
  deriving (Eq, Show, Ord, Generic)

data AppAction = SelectPrevious | SelectNext | EditCurrent
  deriving (Eq, Show, Ord)

data Resources = SideBar | Content Day
  deriving (Eq, Show, Ord)

runApp :: IO ()
runApp = do
  initialAppState <- loadJournalDirectory myAppConfig
  void $ defaultMain (app myAppConfig) initialAppState
  putStrLn ""

myAppConfig :: AppConfig
myAppConfig = AppConfig "/home/damian/code/vimwiki/log"

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime

app :: AppConfig -> App AppState e Resources
app appConfig = App {..}
  where
    appDraw = draw
    appChooseCursor :: s -> [CursorLocation Resources] -> Maybe (CursorLocation Resources)
    appChooseCursor _ _ = Nothing
    appHandleEvent = eventHandler appConfig
    appStartEvent = pure ()
    appAttrMap = pure styleMap

draw :: AppState -> [Widget Resources]
draw appState@(AppState {..}) =
  pure $
    hBox
      [ border $ drawSideBar appState,
        border $ drawContent (current entries) markdown
      ]

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

eventHandler :: AppConfig -> BrickEvent Resources e -> EventM Resources AppState ()
eventHandler appConfig (VtyEvent evt) = case evt of
  V.EvKey KEsc _ -> halt
  V.EvKey (KChar 'q') _ -> halt
  V.EvKey (KChar 'J') _ -> do
    modify $ #entries %~ movePrev
    refreshCurrentFile appConfig
  V.EvKey (KChar 'K') _ -> do
    refreshCurrentFile appConfig
    modify $ #entries %~ moveNext
  V.EvKey (KChar 'j') _ -> increaseScrollContent
  V.EvKey (KChar 'k') _ -> decreaseScrollContent
  V.EvKey (KChar 'e') _ -> editContent appConfig
  _ -> pure ()
eventHandler _ _ = pure ()

refreshCurrentFile :: AppConfig -> EventM Resources AppState ()
refreshCurrentFile appConfig = do
  entries <- gets $ view #entries
  md <- liftIO $ readLogFile $ dayToFilePath appConfig (current entries)
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
  suspendAndResume' $ callProcess "hx" [file]

isMarkdownFile :: FilePath -> Bool
isMarkdownFile file = takeExtension file == ".md"

loadJournalDirectory :: AppConfig -> IO AppState
loadJournalDirectory appConfig@AppConfig {..} = do
  today <- getCurrentDay
  paths <- Relude.filter isMarkdownFile <$> listDirectory appConfigLogPath
  let days = rights $ Parsec.runParser P.day () appConfigLogPath <$> paths
  let days' = NE.reverse $ NE.nub $ today :| sort days
  zippers <- forM days' $ \day -> do
    pure $ Zipper day [] []
  let entries = sconcat zippers
  entryContent <- readLogFile $ dayToFilePath appConfig $ entries ^. #current
  pure $ AppState {entries = entries, markdown = entryContent}

readLogFile :: FilePath -> IO Text
readLogFile file = do
  eContent <- try $ BS.readFile file
  case eContent of
    Left (SomeException e) -> print e >> pure ""
    Right c -> pure $ decodeUtf8With lenientDecode c

dayToFilePath :: AppConfig -> Day -> FilePath
dayToFilePath AppConfig {..} day = appConfigLogPath </> formatShow iso8601Format day FP.<.> "md"
