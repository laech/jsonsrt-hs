{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Jsonish
  ( Jsonish (..),
    parse,
    format,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as TextBuilder
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, many, sepBy, takeWhile1P, try)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char (char, space, string)

type Parser = Parsec Void Text

data Jsonish
  = Value Text
  | Array [Jsonish]
  | Object [(Text, Jsonish)]
  deriving (Show, Eq)

json :: Parser Jsonish
json = space *> (object <|> array <|> value) <* space
  where
    takeWhile1P_ = takeWhile1P Nothing
    token c = space *> char c <* space

    str =
      (\s -> "\"" <> s <> "\"")
        <$> between
          (char '"')
          (char '"')
          (fold <$> many (try (string "\\\"") <|> takeWhile1P_ (/= '"')))

    nonStr =
      takeWhile1P_ (\x -> not (isSpace x) && x `notElem` (",\"{}[]" :: String))

    value = Value <$> (str <|> nonStr)

    object =
      Object
        <$> between
          (token '{')
          (token '}')
          (member `sepBy` token ',')

    member =
      (,)
        <$> str
        <*> (token ':' *> json)

    array =
      Array
        <$> between
          (token '[')
          (token ']')
          (json `sepBy` token ',')

parse :: Text -> Either (ParseErrorBundle Text Void) Jsonish
parse = Parsec.parse json ""

format :: Text -> Either (ParseErrorBundle Text Void) Text
format input =
  LazyText.toStrict . TextBuilder.toLazyText . fmtJson 0 False
    <$> Parsec.parse json "" input
  where
    foldLine = fold . intersperse ",\n"
    indent level = TextBuilder.fromText (Text.replicate level "  ")

    fmtJson level indentFirstLine val =
      (if indentFirstLine then indent level else "") <> case val of
        (Value x) -> TextBuilder.fromText x
        (Array []) -> "[]"
        (Array xs) -> "[\n" <> fmtArr level xs <> "\n" <> indent level <> "]"
        (Object []) -> "{}"
        (Object xs) -> "{\n" <> fmtObj level xs <> "\n" <> indent level <> "}"

    fmtArr level = foldLine . fmap (fmtJson (level + 1) True)
    fmtObj level = foldLine . fmap (fmtMember level)
    fmtMember level (key, val) =
      indent (level + 1)
        <> TextBuilder.fromText key
        <> ": "
        <> fmtJson (level + 1) False val
