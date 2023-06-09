{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Jsonish
  ( Jsonish (..),
    parse,
    format,
    sortByName,
    sortByValue,
  )
where

import Control.Applicative (liftA2, (<|>))
import Control.Arrow (left, second)
import Data.Char (isSpace)
import Data.Foldable (find, fold)
import Data.List (sortBy, sortOn)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, errorBundlePretty, many, sepBy, takeWhile1P)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char qualified as Parsec

type Parser = Parsec Void Text

data Jsonish
  = Value Text
  | Array [Jsonish]
  | Object [(Text, Jsonish)]
  deriving (Show, Eq)

parse :: Text -> Either String Jsonish
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
        <$> string
        <*> (token ':' *> jsonish)

array :: Parser Jsonish
array =
  Array
    <$> between
      (token '[')
      (token ']')
      (jsonish `sepBy` token ',')

value :: Parser Jsonish
value = Value <$> (string <|> other)
  where
    (<||>) = liftA2 (||)
    other = takeWhile1P Nothing (not . (isSpace <||> (`Text.elem` ",:{}[]")))

string :: Parser Text
string = do
  inner <-
    between (char '"') (char '"') . fmap fold . many $
      takeWhile1P Nothing (`notElem` ['\\', '\"'])
        <|> Parsec.string "\\\""
        <|> takeWhile1P Nothing (/= '"')
  pure $ Text.concat ["\"", inner, "\""]

token :: Char -> Parser Char
token c = space *> char c <* space

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
    indent level = Text.take (level * 2) $ Text.cycle " "

sortByName :: Jsonish -> Jsonish
sortByName = \case
  v@(Value _) -> v
  Object xs -> Object . sortOn fst . fmap (second sortByName) $ xs
  Array xs -> Array . fmap sortByName $ xs

sortByValue :: Text -> Jsonish -> Jsonish
sortByValue name = \case
  v@(Value _) -> v
  Object xs -> Object . fmap (second $ sortByValue name) $ xs
  Array xs -> Array . sortBy compareByValue . fmap (sortByValue name) $ xs
  where
    compareByValue a b = case (findValue a, findValue b) of
      (Just (Value x), Just (Value y)) -> compare x y
      _ -> EQ
    findValue = \case
      (Object xs) -> fmap snd . find ((("\"" <> name <> "\"") ==) . fst) $ xs
      _ -> Nothing
