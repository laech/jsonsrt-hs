{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Jsonish
  ( Jsonish (..),
    parse,
  )
where

import Control.Applicative ((<|>))
import Data.Binary (Word8)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Char qualified as Char
import Data.Foldable (fold)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, many, sepBy, takeWhile1P)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Byte (space, string)
import Text.Megaparsec.Byte qualified as Parsec

type Parser = Parsec Void ByteString

data Jsonish
  = Value ByteString
  | Array [Jsonish]
  | Object [(ByteString, Jsonish)]
  deriving (Show, Eq)

parse :: ByteString -> Either (ParseErrorBundle ByteString Void) Jsonish
parse = Parsec.parse jsonish ""

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
