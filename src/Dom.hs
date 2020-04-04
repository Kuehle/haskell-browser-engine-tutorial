module Dom
    ( NodeType
    , Node
    , text
    , Dom.elem
    , AttrMap
    )
where

import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM

data NodeType = Text T.Text
    | Element ElementData

data NTree a = NTree a [NTree a]
    deriving (Show)

type Node = NTree NodeType

type AttrMap = HM.HashMap T.Text T.Text

data ElementData = ElementData T.Text AttrMap

text :: T.Text -> Node
text = flip NTree [] . Text

elem :: T.Text -> AttrMap -> [Node] -> Node
elem name atts = NTree (Element (ElementData name atts))
