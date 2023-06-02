{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Jsonish
  ( Jsonish (..),
    parse,
    format,
    sortByName,
    sortByValue,
  )
where

import Control.Applicative (liftA2, (<|>))
import Control.Arrow (left, second)
import Data.Binary (Word8)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.Char qualified as Char
import Data.Foldable (find, fold)
import Data.List (sortBy, sortOn)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, errorBundlePretty, many, sepBy, takeWhile1P)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Byte (space)
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
    (<&&>) = liftA2 (&&)
    other =
      takeWhile1P Nothing $
        (not . isSpace) <&&> (`BS.notElem` ",:{}[]")

string :: Parser ByteString
string = do
  inner <-
    between (char '"') (char '"') . fmap fold . many $
      takeWhile1P Nothing (`BS.notElem` "\\\"")
        <|> Parsec.string "\\\""
        <|> takeWhile1P Nothing (`BS.notElem` "\"")
  pure $ BS.concat ["\"", inner, "\""]

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

    foldLine = BS.intercalate ",\n"
    indent level = BS.take (level * 2) $ BS.cycle " "

sortByName :: Jsonish -> Jsonish
sortByName = \case
  v@(Value _) -> v
  Object xs -> Object . sortOn fst . fmap (second sortByName) $ xs
  Array xs -> Array . fmap sortByName $ xs

sortByValue :: ByteString -> Jsonish -> Jsonish
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
