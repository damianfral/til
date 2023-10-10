{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.Markdown (drawMarkdown) where

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
    Left _ -> txtWrap md
    Right pandoc -> drawPandoc pandoc
  where
    settings = def {readerExtensions = pandocExtensions}

drawPandoc :: Pandoc -> Widget n
drawPandoc (Pandoc _ blocks) = vBox $ drawPandocBlock <$> blocks

drawParagraph :: [Inline] -> Widget n
drawParagraph paragraph = vBox $ lines' <&> (\line -> hBox $ drawInline <$> line)
  where
    lines' = splitOn [SoftBreak] paragraph

drawPandocBlock :: Block -> Widget n
drawPandocBlock (Plain lines') = drawParagraph lines'
drawPandocBlock (Para paragraph) = padTop (Pad 0) $ padBottom (Pad 1) $ drawParagraph paragraph
drawPandocBlock (LineBlock paragraphs) = vBox $ drawPandocBlock . Para <$> paragraphs
drawPandocBlock (CodeBlock _ code) = do
  padBottom (Pad 1) $ borderBox $ drawPandocBlock . Plain . pure . Str <$> lines'
  where
    lines' = T.lines code
drawPandocBlock (BlockQuote blocks) = borderBox $ drawPandocBlock <$> blocks
drawPandocBlock (OrderedList _ listItems) = drawList ixToString listItems
  where
    ixToString ix = show ix <> "."
drawPandocBlock (BulletList listItems) = drawList (const "*") listItems
drawPandocBlock (Header i _ inlines) =
  padBottom (Pad 1) $
    bold $
      hBox
        [ str $ replicate i '#' <> " ",
          str "",
          hBox $ drawInline <$> inlines
        ]
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
  hBox
    [ str $ getPrefix i <> " ",
      vBox $ drawPandocBlock <$> blocks
    ]

drawInline :: Inline -> Widget n
drawInline (Str t) = txt t
drawInline (Emph ts) = italic $ hBox $ drawInline <$> ts
drawInline (Underline ts) = underline $ vBox $ drawInline <$> ts
drawInline (Strong ts) = bold $ hBox $ drawInline <$> ts
drawInline (Strikeout ts) = withAttr (attrName "strikeout") $ hBox $ drawInline <$> ts
drawInline (Quoted _ ts) = italic $ hBox $ drawInline <$> Str "> " : ts
drawInline (Cite _ ts) = drawInline $ Quoted SingleQuote ts
drawInline (Code _ t) = drawInline $ Str t
drawInline Space = str " "
drawInline SoftBreak = str " "
drawInline LineBreak = str "\n"
drawInline (Link _ inline (url, _)) =
  underline $
    hyperlink url $
      if null inline
        then txt url
        else hBox $ (drawInline <$> inline) <> [txt $ " " <> url]
drawInline _ = emptyWidget
