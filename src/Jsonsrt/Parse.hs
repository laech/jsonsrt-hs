module Jsonsrt.Parse (parse) where

import Control.Applicative (Applicative (liftA2), (<|>))
import Control.Arrow (left)
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Void (Void)
import Jsonsrt.Node (Node (..))
import Text.Megaparsec (Parsec, between, errorBundlePretty, many, sepBy, takeWhile1P)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char qualified as Parsec

type Parser = Parsec Void Text

parse :: Text -> Either String Node
parse = left errorBundlePretty . Parsec.parse node ""

node :: Parser Node
node = space *> (object <|> array <|> value) <* space

object :: Parser Node
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
        <*> (token ':' *> node)

array :: Parser Node
array =
  Array
    <$> between
      (token '[')
      (token ']')
      (node `sepBy` token ',')

value :: Parser Node
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
