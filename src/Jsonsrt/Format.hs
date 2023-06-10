module Jsonsrt.Format (format) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Jsonsrt.Node (Node (..))

format :: Node -> Text
format = fmtNode 0 False
  where
    fmtNode level indentFirstLine val =
      (if indentFirstLine then indent level else "") <> case val of
        (Value x) -> x
        (Array []) -> "[]"
        (Array xs) -> "[\n" <> fmtArr level xs <> "\n" <> indent level <> "]"
        (Object []) -> "{}"
        (Object xs) -> "{\n" <> fmtObj level xs <> "\n" <> indent level <> "}"

    fmtArr level = foldLine . fmap (fmtNode (level + 1) True)
    fmtObj level = foldLine . fmap (fmtMember level)
    fmtMember level (key, val) =
      indent (level + 1)
        <> key
        <> ": "
        <> fmtNode (level + 1) False val

    foldLine = Text.intercalate ",\n"
    indent level = Text.take (level * 2) $ Text.cycle " "
