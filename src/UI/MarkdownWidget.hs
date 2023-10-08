{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.MarkdownWidget where

import Brick
import Brick.Widgets.Border (border)
import Data.Default
import Data.List.Split (splitOn)
import Relude
import Text.Pandoc
import Text.Wrap

wrapSettings :: WrapSettings
wrapSettings =
  defaultWrapSettings
    { preserveIndentation = True,
      fillStrategy = FillPrefix ""
    }

drawMarkdown :: Text -> Widget n
drawMarkdown md = do
  let result = runPure $ readMarkdown def $ wrapText wrapSettings 80 md
  case result of
    Left _ -> txt md
    Right pandoc -> drawPandoc pandoc

drawPandoc :: Pandoc -> Widget n
drawPandoc (Pandoc _ blocks) = vBox $ drawPandocBlock <$> blocks

drawPandocBlock :: Block -> Widget n
drawPandocBlock (Plain lines') = hBox $ drawInline <$> lines'
drawPandocBlock (Para paragraph) = do
  padTop (Pad 1) $ padBottom (Pad 1) $ vBox linesWidgets
  where
    lines' = splitOn [SoftBreak] paragraph
    linesWidgets = lines' <&> (\line -> hBox $ drawInline <$> line)
drawPandocBlock (LineBlock paragraphs) = drawPandocBlock $ Para $ mconcat paragraphs
-- borderBox $ mconcat $ fmap drawInline <$> paragraphs
drawPandocBlock (CodeBlock _ code) = borderBox $ pure $ txt code
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
drawInline (Emph ts) = withAttr (attrName "italic") $ vBox $ drawInline <$> ts
drawInline (Underline ts) = withAttr (attrName "underline") $ vBox $ drawInline <$> ts
drawInline (Strong ts) = withAttr (attrName "strong") $ vBox $ drawInline <$> ts
drawInline (Strikeout ts) = withAttr (attrName "strikeout") $ vBox $ drawInline <$> ts
drawInline (Quoted _ ts) = withAttr (attrName "aaaa") $ vBox $ drawInline <$> ts
drawInline (Cite _ ts) = drawInline $ Quoted SingleQuote ts
drawInline (Code _ t) = drawInline $ Str t
drawInline Space = str " "
-- drawInline SoftBreak = str ""
-- drawInline LineBreak = str "\n"
drawInline (Link _ _ (url, title)) = hyperlink url $ txt $ if title == "" then url else title
drawInline _ = emptyWidget
