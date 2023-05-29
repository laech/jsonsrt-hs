module Data.Jsonish.Format (format) where

import Data.Jsonish (Jsonish (Array, Object, Value))
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text

format :: Jsonish -> Text
format = fmtJsonish 0 False
  where
    fmtJsonish level indentFirstLine val =
      (if indentFirstLine then indent level else "") <> case val of
        (Value x) -> x
        (Array []) -> "[]"
        (Array xs) -> "[\n" <> fmtArr level xs <> "\n" <> indent level <> "]"
        (Object []) -> "{}"
        (Object xs) -> "{\n" <> fmtObj level xs <> "\n" <> indent level <> "}"

    fmtArr level = foldLine . fmap (fmtJsonish (level + 1) True)
    fmtObj level = foldLine . fmap (fmtMember level)
    fmtMember level (key, val) =
      indent (level + 1)
        <> key
        <> ": "
        <> fmtJsonish (level + 1) False val

    foldLine = Text.intercalate ",\n"
    indent level = Text.replicate level "  "
