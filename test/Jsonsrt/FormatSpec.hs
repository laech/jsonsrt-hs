{-# LANGUAGE QuasiQuotes #-}

module Jsonsrt.FormatSpec (spec) where

import Control.Monad (forM_)
import Data.String.Interpolate (i)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Jsonsrt.Format (format)
import Jsonsrt.Parse (parse)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

escapeNewline :: Text -> String
escapeNewline = Text.unpack . Text.replace "\n" "\\n"

spec :: Spec
spec = do
  describe "format" $ do
    forM_ formatterTests $ \(input, expected) ->
      it ("can pretty print: `" ++ escapeNewline input ++ "`") $
        either
          expectationFailure
          (`shouldBe` expected)
          (format <$> parse input)

formatterTests :: [(Text, Text)]
formatterTests =
  [ ("null", "null"),
    (" true", "true"),
    ("false ", "false"),
    (" 1 ", "1"),
    ("\t-2", "-2"),
    ("-3e10\n", "-3e10"),
    ("{}", "{}"),
    ("[]", "[]"),
    ( [i|{"a":"hello"}|],
      [i|{
  "a": "hello"
}|]
    ),
    ( [i|{"a":"hello", "b":  [1, 2 , false]}|],
      [i|{
  "a": "hello",
  "b": [
    1,
    2,
    false
  ]
}|]
    ),
    ( [i|["a", "hello", null, { "i": "x"}, -1.000 ]|],
      [i|[
  "a",
  "hello",
  null,
  {
    "i": "x"
  },
  -1.000
]|]
    )
  ]
