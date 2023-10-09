module UI.Style (styleMap, selected, italic, bold, dim, underline) where

import Brick
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V

selectedAttrName :: AttrName
selectedAttrName = attrName "selected"

italicAttrName :: AttrName
italicAttrName = attrName "italic"

boldAttrName :: AttrName
boldAttrName = attrName "bold"

underlineAttrName :: AttrName
underlineAttrName = attrName "underline"

dimAttrName :: AttrName
dimAttrName = attrName "dim"

styleMap :: A.AttrMap
styleMap =
  A.attrMap
    V.defAttr
    [ (selectedAttrName, fg V.yellow),
      (italicAttrName, V.defAttr `V.withStyle` V.italic),
      (boldAttrName, V.defAttr `V.withStyle` V.bold),
      (underlineAttrName, V.defAttr `V.withStyle` V.underline),
      (dimAttrName, V.defAttr `V.withStyle` V.bold)
    ]

selected :: Widget n -> Widget n
selected = withAttr selectedAttrName

underline :: Widget n -> Widget n
underline = withAttr underlineAttrName

dim :: Widget n -> Widget n
dim = withAttr dimAttrName

bold :: Widget n -> Widget n
bold = withAttr boldAttrName

italic :: Widget n -> Widget n
italic = withAttr italicAttrName
