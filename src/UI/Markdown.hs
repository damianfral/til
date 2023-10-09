{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.Markdown where

import Brick
import Brick.Widgets.Border (border)
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
    Left _ -> txt md
    Right pandoc -> drawPandoc pandoc
  where
    settings = def {readerExtensions = pandocExtensions}

drawPandoc :: Pandoc -> Widget n
drawPandoc (Pandoc _ blocks) = vBox $ drawPandocBlock <$> blocks

drawPandocBlock :: Block -> Widget n
drawPandocBlock (Plain lines') = hBox $ drawInline <$> lines'
drawPandocBlock (Para paragraph) = do
  padTop (Pad 1) $ padBottom (Pad 1) $ vBox linesWidgets
  where
    lines' = splitOn [SoftBreak] paragraph
    linesWidgets = lines' <&> (\line -> hBox $ drawInline <$> line)
drawPandocBlock (LineBlock paragraphs) = vBox $ drawPandocBlock . Para <$> paragraphs
-- borderBox $ mconcat $ fmap drawInline <$> paragraphs
drawPandocBlock (CodeBlock _ code) = do
  borderBox $ drawPandocBlock . Para . pure . Str <$> lines'
  where
    lines' = T.lines code
drawPandocBlock (BlockQuote blocks) = borderBox $ drawPandocBlock <$> blocks
drawPandocBlock (OrderedList _ listItems) = drawList show listItems
drawPandocBlock (BulletList listItems) = drawList (const "*") listItems
drawPandocBlock (Header i _ inlines) =
  padTop (Pad 1) $
    padBottom (Pad 1) $
      hBox
        [ str $ replicate i '#' <> " ",
          str "",
          hBox $ drawInline <$> inlines
        ]
drawPandocBlock _ = emptyWidget

borderBox :: [Widget n] -> Widget n
borderBox = border . vBox

drawList :: (Int -> String) -> [[Block]] -> Widget n
drawList getPrefix listItems = vBox $ uncurry (drawListItem getPrefix) <$> zip [1 ..] listItems

drawListItem :: (Int -> String) -> Int -> [Block] -> Widget n
drawListItem getPrefix i blocks =
  hBox
    [ str $ getPrefix i <> " ",
      vBox $ drawPandocBlock <$> blocks
    ]

drawInline :: Inline -> Widget n
drawInline (Str t) = txt t
drawInline (Emph ts) = italic $ vBox $ drawInline <$> ts
drawInline (Underline ts) = underline $ vBox $ drawInline <$> ts
drawInline (Strong ts) = bold $ vBox $ drawInline <$> ts
drawInline (Strikeout ts) = withAttr (attrName "strikeout") $ vBox $ drawInline <$> ts
drawInline (Quoted _ ts) = withAttr (attrName "aaaa") $ vBox $ drawInline <$> ts
drawInline (Cite _ ts) = drawInline $ Quoted SingleQuote ts
drawInline (Code _ t) = drawInline $ Str t
drawInline Space = str " "
-- drawInline SoftBreak = str ""
-- drawInline LineBreak = str "\n"
drawInline (Link _ inline (url, _)) =
  hyperlink url $
    if null inline then txt url else drawPandocBlock (Plain inline)
drawInline _ = emptyWidget
