{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI (runApp) where

import Brick
import qualified Brick.AttrMap ()
import qualified Brick.AttrMap as A
import Brick.Widgets.Border
import Control.Exception (try)
import qualified Data.ByteString as BS
import Data.GenValidity.Text ()
import qualified Data.List.NonEmpty as NE
import Data.Text hiding (elem, reverse, take)
import qualified Data.Text as T
import Data.Text.IO
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.Time.Parsers as P
import Graphics.Vty (Key (..))
import qualified Graphics.Vty as V
import Relude hiding (on, putStrLn)
import System.Directory
import System.FilePath (takeExtension, (<.>), (</>))
import System.Process
import qualified Text.Parsec as Parsec
import Text.Wrap

--------------------------------------------------------------------------------

data Zipper a = Zipper
  { current :: a,
    previous :: [a],
    next :: [a]
  }
  deriving stock (Show, Eq, Ord, Generic, Functor)

zipperToNEList :: Zipper a -> NonEmpty a
zipperToNEList Zipper {..} = case reverse previous of
  [] -> current :| next
  p : ps -> p :| (ps <> [current] <> next)

zipperToList :: Zipper a -> [a]
zipperToList = NE.toList . zipperToNEList

instance Semigroup (Zipper a) where
  z1 <> z2 = z2 {previous = zipperToList z1 <> previous z2}

moveNext :: Zipper a -> Zipper a
moveNext z@(Zipper _ _ []) = z
moveNext (Zipper current previous next) = Zipper current' previous' next'
  where
    (current' : next') = next
    previous' = current : previous

movePrev :: Zipper a -> Zipper a
movePrev z@(Zipper _ [] _) = z
movePrev (Zipper current previous next) = Zipper current' previous' next'
  where
    (current' : previous') = previous
    next' = current : next

--------------------------------------------------------------------------------

data Entry = Entry
  { entryDay :: Day,
    entryFile :: FilePath,
    entryContent :: Text
    -- entryScrollOffset :: Int
  }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

type AppState = Zipper Entry

data AppAction = SelectPrevious | SelectNext | EditCurrent
  deriving (Eq, Show, Ord)

data Resources = SideBar | Content Day
  deriving (Eq, Show, Ord)

runApp :: IO ()
runApp = do
  initialAppState <- loadJournalDirectory "/home/damian/code/vimwiki/log"
  st <- defaultMain app initialAppState
  print st
  putStrLn ""

getCurrentDay :: IO Day
getCurrentDay = do
  localDay . zonedTimeToLocalTime <$> getZonedTime

selected :: AttrName
selected = attrName "selected"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr [(selected, fg V.yellow)]

app :: App AppState e Resources
app = App {..}
  where
    appDraw = draw
    appChooseCursor :: s -> [CursorLocation Resources] -> Maybe (CursorLocation Resources)
    appChooseCursor _ _ = Nothing
    appHandleEvent = eventHandler
    appStartEvent = pure ()
    appAttrMap = pure theMap

draw :: AppState -> [Widget Resources]
draw appState@(Zipper {..}) =
  pure $
    hBox
      [ border $ drawSideBar appState,
        border $ drawMarkdown (entryDay current) $ entryContent current
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
                [withAttr selected $ visible $ drawEntry current],
                drawEntry <$> previous
              ]

drawMarkdown :: Day -> Text -> Widget Resources
drawMarkdown day =
  withVScrollBars OnRight
    . viewport (Content day) Vertical
    . padAll 1
    -- . vBox
    . txtWrapWith wrapSettings
  where
    wrapSettings =
      defaultWrapSettings
        { preserveIndentation = True,
          fillStrategy = FillPrefix "↪ "
        }

drawEntry :: Entry -> Widget n
drawEntry Entry {..} = hBox [txt $ show entryDay]

eventHandler :: BrickEvent Resources e -> EventM Resources AppState ()
eventHandler (VtyEvent evt) = case evt of
  V.EvKey KEsc _ -> halt
  V.EvKey (KChar 'q') _ -> halt
  V.EvKey (KChar 'J') _ -> modify movePrev
  V.EvKey (KChar 'K') _ -> modify moveNext
  V.EvKey (KChar 'j') _ ->
    gets current >>= \Entry {..} -> vScrollBy (viewportScroll $ Content entryDay) 1
  V.EvKey (KChar 'k') _ ->
    gets current >>= \Entry {..} -> vScrollBy (viewportScroll $ Content entryDay) (-1)
  V.EvKey (KChar 'e') _ ->
    gets current >>= \Entry {..} ->
      suspendAndResume' $ callProcess "$EDITOR" [entryFile]
  _ -> pure ()
eventHandler _ = pure ()

isMarkdownFile :: FilePath -> Bool
isMarkdownFile file = takeExtension file == ".md"

loadJournalDirectory :: FilePath -> IO AppState
loadJournalDirectory fp = do
  today <- getCurrentDay
  paths <- Relude.filter isMarkdownFile <$> listDirectory fp
  let days = rights $ Parsec.runParser P.day () fp <$> paths
  let days' = NE.reverse $ NE.nub $ today :| reverse (sort days)
  zippers <- forM days' $ \entryDay -> do
    let entryFile = fp </> dayToMarkdownFilename entryDay
    eContent <- try $ BS.readFile entryFile
    entryContent <- case eContent of
      Left (SomeException e) -> print e >> pure ""
      Right c -> pure $ decodeUtf8With lenientDecode c
    let entry = Entry entryDay entryFile entryContent
    pure $ Zipper entry [] []
  pure $ sconcat zippers

mdr file = T.pack <$> readProcess "some-md-linter" [file] ""

dayToMarkdownFilename :: Day -> FilePath
dayToMarkdownFilename day = formatShow iso8601Format day <.> "md"
