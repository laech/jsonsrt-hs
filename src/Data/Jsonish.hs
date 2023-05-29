{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Jsonish
  ( Jsonish (..),
    parse,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, many, sepBy, takeWhile1P)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char (char, space, string)

type Parser = Parsec Void Text

data Jsonish
  = Value Text
  | Array [Jsonish]
  | Object [(Text, Jsonish)]
  deriving (Show, Eq)

parse :: Text -> Either (ParseErrorBundle Text Void) Jsonish
parse = Parsec.parse jsonish ""

jsonish :: Parser Jsonish
jsonish = space *> (object <|> array <|> value) <* space

object :: Parser Jsonish
object = Object <$> between (token '{') (token '}') (member `sepBy` token ',')
  where
    member = (,) <$> valueContent <*> (token ':' *> jsonish)

array :: Parser Jsonish
array = Array <$> between (token '[') (token ']') (jsonish `sepBy` token ',')

value :: Parser Jsonish
value = Value <$> valueContent

valueContent :: Parser Text
valueContent = str <|> notStr
  where
    notStr = takeWhile1P Nothing $ \x -> not (isSpace x || Text.elem x ",:{}[]")
    str = do
      inner <-
        between (char '"') (char '"') . fmap fold . many $
          takeWhile1P Nothing (not . (`Text.elem` "\\\""))
            <|> string "\\\""
            <|> takeWhile1P Nothing (not . (`Text.elem` "\""))
      pure $ Text.concat ["\"", inner, "\""]

token :: Char -> Parser Char
token s = space *> char s <* space
