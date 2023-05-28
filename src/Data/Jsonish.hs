{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Jsonish
  ( Jsonish (..),
    parse,
  )
where

import Control.Applicative ((<|>))
import Data.Binary (Word8)
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, many, sepBy, takeWhile1P, try)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Byte (char, space, string)

type Parser = Parsec Void LazyByteString

data Jsonish
  = Value StrictByteString
  | Array [Jsonish]
  | Object [(StrictByteString, Jsonish)]
  deriving (Show, Eq)

parse :: LazyByteString -> Either (ParseErrorBundle LazyByteString Void) Jsonish
parse = Parsec.parse jsonish ""
  where
    jsonish =
      space
        *> ( Object <$> obj
               <|> Array <$> arr
               <|> Value <$> val
           )
        <* space

    arr =
      between
        (token '[')
        (token ']')
        (jsonish `sepBy` token ',')

    obj =
      between
        (token '{')
        (token '}')
        (member `sepBy` token ',')

    member = (,) <$> val <*> (token ':' *> jsonish)

    val = str <|> notStr

    str =
      (\s -> "\"" <> s <> "\"")
        <$> between
          (char . toEnum . fromEnum $ '"')
          (char . toEnum . fromEnum $ '"')
          ( fmap fold . many . fmap LBS.toStrict . try $
              string "\\\"" <|> takeWhile1P_ (/= (toEnum . fromEnum $ '"'))
          )

    notStr =
      fmap LBS.toStrict . takeWhile1P_ $ \x ->
        not (isSpace . toEnum . fromEnum $ x) && not (LBS.elem x ",:{}[]")

    takeWhile1P_ = takeWhile1P Nothing

    token :: Char -> Parser Word8
    token c = space *> (char . toEnum . fromEnum $ c) <* space
