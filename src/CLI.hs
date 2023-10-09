{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI (runApp) where

import Brick
import qualified Brick.AttrMap ()
import Brick.Widgets.Border
import Control.Exception (try)
import qualified Data.ByteString as BS
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
import System.FilePath (takeExtension, (<.>), (</>))
import System.Process
import qualified Text.Parsec as Parsec
import UI.Markdown (drawMarkdown)
import UI.Style

--------------------------------------------------------------------------------

type AppState = Zipper Entry

data AppAction = SelectPrevious | SelectNext | EditCurrent
  deriving (Eq, Show, Ord)

data Resources = SideBar | Content Day
  deriving (Eq, Show, Ord)

runApp :: IO ()
runApp = do
  initialAppState <- loadJournalDirectory "/home/damian/code/vimwiki/log"
  void $ defaultMain app initialAppState
  putStrLn ""

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime

app :: App AppState e Resources
app = App {..}
  where
    appDraw = draw
    appChooseCursor :: s -> [CursorLocation Resources] -> Maybe (CursorLocation Resources)
    appChooseCursor _ _ = Nothing
    appHandleEvent = eventHandler
    appStartEvent = pure ()
    appAttrMap = pure styleMap

draw :: AppState -> [Widget Resources]
draw appState@(Zipper {..}) =
  pure $
    hBox
      [ border $ drawSideBar appState,
        border $ drawContent (entryDay current) $ entryContent current
      ]

drawSideBar :: AppState -> Widget Resources
drawSideBar Zipper {..} =
  hLimit 13 $
    withVScrollBars OnRight $
      viewport SideBar Vertical $
        padLeft (Pad 1) $
          vBox $
            mconcat
              [ drawEntry <$> reverse next,
                [selected $ visible $ drawEntry current],
                drawEntry <$> previous
              ]

drawContent :: Day -> Text -> Widget Resources
drawContent day =
  withVScrollBars OnRight
    . viewport (Content day) Vertical
    . padAll 1
    . drawMarkdown

drawEntry :: Entry -> Widget n
drawEntry Entry {..} = hBox [txt $ show entryDay]

eventHandler :: BrickEvent Resources e -> EventM Resources AppState ()
eventHandler (VtyEvent evt) = case evt of
  V.EvKey KEsc _ -> halt
  V.EvKey (KChar 'q') _ -> halt
  V.EvKey (KChar 'J') _ -> modify movePrev
  V.EvKey (KChar 'K') _ -> modify moveNext
  V.EvKey (KChar 'j') _ -> increaseScrollContent
  V.EvKey (KChar 'k') _ -> decreaseScrollContent
  V.EvKey (KChar 'e') _ -> editContent
  _ -> pure ()
eventHandler _ = pure ()

increaseScrollContent :: EventM Resources (Zipper Entry) ()
increaseScrollContent = do
  Entry {..} <- gets current
  let resource = Content entryDay
  vScrollBy (viewportScroll resource) 1

decreaseScrollContent :: EventM Resources (Zipper Entry) ()
decreaseScrollContent = do
  Entry {..} <- gets current
  let resource = Content entryDay
  vScrollBy (viewportScroll resource) (-1)

editContent :: EventM Resources (Zipper Entry) ()
editContent = do
  Entry {..} <- gets current
  suspendAndResume' $ callProcess "hx" [entryFile]

isMarkdownFile :: FilePath -> Bool
isMarkdownFile file = takeExtension file == ".md"

loadJournalDirectory :: FilePath -> IO AppState
loadJournalDirectory fp = do
  today <- getCurrentDay
  paths <- Relude.filter isMarkdownFile <$> listDirectory fp
  let days = rights $ Parsec.runParser P.day () fp <$> paths
  let days' = NE.reverse $ NE.nub $ today :| sort days
  zippers <- forM days' $ \entryDay -> do
    let entryFile = fp </> dayToMarkdownFilename entryDay
    eContent <- try $ BS.readFile entryFile
    entryContent <- case eContent of
      Left (SomeException e) -> print e >> pure ""
      Right c -> pure $ decodeUtf8With lenientDecode c
    let entry = Entry entryDay entryFile entryContent
    pure $ Zipper entry [] []
  pure $ sconcat zippers

dayToMarkdownFilename :: Day -> FilePath
dayToMarkdownFilename day = formatShow iso8601Format day <.> "md"
