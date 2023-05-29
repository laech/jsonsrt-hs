module Data.Jsonish.Format (format) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Jsonish (Jsonish (Array, Object, Value))

format :: Jsonish -> ByteString
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

    foldLine = ByteString.intercalate ",\n"
    indent level = ByteString.take (level * 2) $ ByteString.cycle " "
