{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.View.Markdown (drawMarkdown) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Table
import Data.Default
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Relude
import Text.Pandoc
import Text.Wrap
import UI.Style

wrapSettings :: WrapSettings
wrapSettings =
  defaultWrapSettings
    { preserveIndentation = True,
      fillStrategy = FillPrefix ""
    }

drawMarkdown :: Text -> Widget n
drawMarkdown md = do
  let result = runPure $ readMarkdown settings $ wrapText wrapSettings 80 md
  case result of
    Left _ -> txtWrap md
    Right pandoc -> drawPandoc pandoc
  where
    settings = def {readerExtensions = pandocExtensions}

drawPandoc :: Pandoc -> Widget n
drawPandoc (Pandoc _ blocks) = vBox $ drawPandocBlock <$> blocks

drawParagraph :: [Inline] -> Widget n
drawParagraph paragraph = vBox $ hBox . fmap drawInline <$> lines'
  where
    lines' = splitOn [SoftBreak] paragraph

drawPandocBlock :: Block -> Widget n
drawPandocBlock (Plain lines') = drawParagraph lines'
drawPandocBlock (Para paragraph) = pb1 $ drawParagraph paragraph
drawPandocBlock (LineBlock paragraphs) =
  vBox $ drawPandocBlock . Para <$> paragraphs
drawPandocBlock (CodeBlock _ code) =
  pb1 $ borderBox $ drawPandocBlock . Plain . pure . Str <$> T.lines code
drawPandocBlock (BlockQuote blocks) = borderBox $ drawPandocBlock <$> blocks
drawPandocBlock (OrderedList _ listItems) = drawList ixToString listItems
  where
    ixToString ix = show ix <> "."
drawPandocBlock (BulletList listItems)
  | any hasCheckbox listItems = drawTaskList listItems
  | otherwise = drawList (const "-") listItems
drawPandocBlock (Header i _ inlines) =
  pb1 $ bold $ hBox [str prefix, hBox $ drawInline <$> inlines]
  where
    prefix = replicate i '#' <> " "
drawPandocBlock (Table _ __ _ tableHead tableBodies _) =
  drawTable headRows $ mconcat bodyRows
  where
    (TableHead _ headRows) = tableHead
    bodyRows = tableBodies <&> \(TableBody _ _ rows1 rows2) -> rows1 <> rows2
drawPandocBlock _ = emptyWidget

borderBox :: [Widget n] -> Widget n
borderBox = border . vBox

drawList :: (Int -> String) -> [[Block]] -> Widget n
drawList getPrefix listItems =
  padBottom (Pad 1) $ vBox $ uncurry (drawListItem getPrefix) <$> ixItems
  where
    ixItems = zip [1 ..] listItems

drawListItem :: (Int -> String) -> Int -> [Block] -> Widget n
drawListItem getPrefix i blocks =
  hBox [str $ getPrefix i <> " ", vBox $ drawPandocBlock <$> blocks]

hasCheckbox :: [Block] -> Bool
hasCheckbox (Plain (RawInline (Format "html") s : _) : _) =
  "checkbox" `T.isInfixOf` s
hasCheckbox (Para (RawInline (Format "html") s : _) : _) =
  "checkbox" `T.isInfixOf` s
hasCheckbox _ = False

isChecked :: [Block] -> Bool
isChecked (Plain (RawInline (Format "html") s : _) : _) =
  "checked" `T.isInfixOf` s
isChecked (Para (RawInline (Format "html") s : _) : _) =
  "checked" `T.isInfixOf` s
isChecked _ = False

stripCheckbox :: [Block] -> [Block]
stripCheckbox (Plain (RawInline (Format "html") _ : Space : rest) : bs) =
  Plain rest : bs
stripCheckbox (Para (RawInline (Format "html") _ : Space : rest) : bs) =
  Para rest : bs
stripCheckbox (Plain (RawInline (Format "html") _ : rest) : bs) =
  Plain rest : bs
stripCheckbox (Para (RawInline (Format "html") _ : rest) : bs) =
  Para rest : bs
stripCheckbox bs = bs

drawTaskList :: [[Block]] -> Widget n
drawTaskList = padBottom (Pad 1) . vBox . fmap drawTaskListItem

drawTaskListItem :: [Block] -> Widget n
drawTaskListItem blocks =
  let prefix = if isChecked blocks then "\x2B24 " else "\x25EF "
      cleanBlocks = stripCheckbox blocks
   in hBox [str prefix, vBox $ drawPandocBlock <$> cleanBlocks]

drawInline :: Inline -> Widget n
drawInline (Str t) = txt t
drawInline (Emph ts) = italic $ hBox $ drawInline <$> ts
drawInline (Underline ts) = underline $ vBox $ drawInline <$> ts
drawInline (Strong ts) = bold $ hBox $ drawInline <$> ts
drawInline (Strikeout ts) = strikethrough $ hBox $ drawInline <$> ts
drawInline (Quoted _ ts) = italic $ hBox $ drawInline <$> Str "> " : ts
drawInline (Cite _ ts) = drawInline $ Quoted SingleQuote ts
drawInline (Code _ t) = txt t
drawInline Space = txt " "
drawInline SoftBreak = txt " "
drawInline LineBreak = txt "\n"
drawInline (Link _ inline (url, _)) = underline $ hyperlink url linkWidget
  where
    linkWidget = hBox $ (drawInline <$> inline) <> [txt $ " " <> url]
drawInline _ = emptyWidget

drawTable :: [Row] -> [Row] -> Widget n
drawTable headRows bodyRows = pb1 $ renderTable $ table rows
  where
    rows = mconcat [fmap bold . drawRow <$> headRows, drawRow <$> bodyRows]

drawRow :: Row -> [Widget n]
drawRow (Row _ cells) = hBox . fmap drawPandocBlock <$> blocks
  where
    blocks = cells <&> \(Cell _ _ _ _ bs) -> bs
