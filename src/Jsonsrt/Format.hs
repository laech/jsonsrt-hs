module Jsonsrt.Format (format) where

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (fromLazyText, fromString, toLazyText)
import Jsonsrt.Node (Node (..))

format :: Node -> Text
format = toLazyText . fmtNode 0 False
  where
    fmtNode level prefix val =
      (if prefix then indent level else "") <> case val of
        (Value x) -> fromLazyText x
        (Array []) -> "[]"
        (Array xs) -> "[\n" <> fmtArr level xs <> "\n" <> indent level <> "]"
        (Object []) -> "{}"
        (Object xs) -> "{\n" <> fmtObj level xs <> "\n" <> indent level <> "}"

    fmtArr level = foldLine . fmap (fmtNode (level + 1) True)
    fmtObj level = foldLine . fmap (fmtMember level)
    fmtMember level (key, val) =
      indent (level + 1)
        <> fromLazyText key
        <> ": "
        <> fmtNode (level + 1) False val

    foldLine = fold . intersperse ",\n"
    indent level = fromString $ replicate (level * 2) ' '
