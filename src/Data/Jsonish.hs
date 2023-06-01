{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Jsonish
  ( Jsonish (..),
    parse,
    format,
    sortByName,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (left, second)
import Data.Binary (Word8)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Char qualified as Char
import Data.Foldable (fold)
import Data.List qualified as List
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, errorBundlePretty, many, sepBy, takeWhile1P)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Byte (space, string)
import Text.Megaparsec.Byte qualified as Parsec

type Parser = Parsec Void ByteString

data Jsonish
  = Value ByteString
  | Array [Jsonish]
  | Object [(ByteString, Jsonish)]
  deriving (Show, Eq)

parse :: ByteString -> Either String Jsonish
parse = left errorBundlePretty . Parsec.parse jsonish ""

jsonish :: Parser Jsonish
jsonish = space *> (object <|> array <|> value) <* space

object :: Parser Jsonish
object =
  Object
    <$> between
      (token '{')
      (token '}')
      (member `sepBy` token ',')
  where
    member =
      (,)
        <$> stringish
        <*> (token ':' *> jsonish)

array :: Parser Jsonish
array =
  Array
    <$> between
      (token '[')
      (token ']')
      (jsonish `sepBy` token ',')

value :: Parser Jsonish
value = Value <$> stringish

stringish :: Parser ByteString
stringish = quoted <|> unquoted
  where
    unquoted = takeWhile1P Nothing $ \x -> not (isSpace x || ByteString.elem x ",:{}[]")
    quoted = do
      inner <-
        between (char '"') (char '"') . fmap fold . many $
          takeWhile1P Nothing (`ByteString.notElem` "\\\"")
            <|> string "\\\""
            <|> takeWhile1P Nothing (`ByteString.notElem` "\"")
      pure $ ByteString.concat ["\"", inner, "\""]

char :: Char -> Parser Word8
char = Parsec.char . toEnum . fromEnum

token :: Char -> Parser Word8
token c = space *> char c <* space

isSpace :: Word8 -> Bool
isSpace = Char.isSpace . toEnum . fromEnum

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

sortByName :: Jsonish -> Jsonish
sortByName = \case
  Value x -> Value x
  Array xs -> Array . fmap sortByName $ xs
  Object xs -> Object . List.sortOn fst . fmap (second sortByName) $ xs
