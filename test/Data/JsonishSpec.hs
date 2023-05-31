{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.JsonishSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString)
import Data.Jsonish (Jsonish (..), parse)
import Data.Jsonish.Format (format)
import Data.String.Interpolate (i)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

escapeNewline :: ByteString -> String
escapeNewline = Text.unpack . Text.replace "\n" "\\n" . Text.decodeUtf8

spec :: Spec
spec = do
  describe "parse" $ do
    forM_ parserTests $ \(input, expected) -> do
      it ("can parse: `" ++ escapeNewline input ++ "`") $
        either
          expectationFailure
          (`shouldBe` expected)
          (parse input)

    forM_ parserTests $ \(input, expected) -> do
      it ("can parse from self formatted output: `" ++ escapeNewline input ++ "`") $
        either
          expectationFailure
          (`shouldBe` expected)
          (parse input >>= parse . format)

  describe "format" $ do
    forM_ formatterTests $ \(src, expected) ->
      it ("can pretty print: `" ++ escapeNewline src ++ "`") $
        either
          expectationFailure
          (`shouldBe` expected)
          (format <$> parse src)

parserTests :: [(ByteString, Jsonish)]
parserTests =
  [ ("true", Value "true"),
    (" true", Value "true"),
    (" true ", Value "true"),
    ("true ", Value "true"),
    ("false\t", Value "false"),
    ("\nfalse\t", Value "false"),
    ("null", Value "null"),
    ("1", Value "1"),
    ("-2", Value "-2"),
    ("-3.4", Value "-3.4"),
    ("5e6", Value "5e6"),
    ("7.00", Value "7.00"),
    ("-8.900", Value "-8.900"),
    (" -10", Value "-10"),
    (" 11 ", Value "11"),
    ("12\t", Value "12"),
    ("\n\t13\n", Value "13"),
    ("\"\"", Value "\"\""),
    (" \"\"", Value "\"\""),
    (" \"\" ", Value "\"\""),
    ("\"\" ", Value "\"\""),
    (" \" \" ", Value "\" \""),
    (" \"a b\" ", Value "\"a b\""),
    (" \"\\\"a b\" ", Value "\"\\\"a b\""),
    (" \"a\\\" b\" ", Value "\"a\\\" b\""),
    (" \"a b\\\"\" ", Value "\"a b\\\"\""),
    (" \"a\nb\" ", Value "\"a\nb\""),
    (" \"\ta \nb false\" ", Value "\"\ta \nb false\""),
    ("[]", Array []),
    (" []", Array []),
    (" [] ", Array []),
    (" [ ] ", Array []),
    ("[ ] ", Array []),
    ("[] ", Array []),
    ("{}", Object []),
    ("{} ", Object []),
    ("{ } ", Object []),
    (" { } ", Object []),
    (" {} ", Object []),
    (" {}", Object []),
    ("[1] ", Array [Value "1"]),
    ("[ 1, false] ", Array [Value "1", Value "false"]),
    ("[ 0E-18 , true ] ", Array [Value "0E-18", Value "true"]),
    ("[ 2 , true , {}] ", Array [Value "2", Value "true", Object []]),
    ( "[\t{},{} , {} , {}\n, []] ",
      Array
        [ Object [],
          Object [],
          Object [],
          Object [],
          Array []
        ]
    ),
    ("{\"hi\" : true} ", Object [("\"hi\"", Value "true")]),
    ("{\"hello world\" : {}} ", Object [("\"hello world\"", Object [])]),
    ("{\"bob\" : []} ", Object [("\"bob\"", Array [])]),
    ( "{\"bob\" : { \"ja\tck\": [1, -3, true, {\"a\" : false}]}} ",
      Object
        [ ( "\"bob\"",
            Object
              [ ( "\"ja\tck\"",
                  Array
                    [ Value "1",
                      Value "-3",
                      Value "true",
                      Object [("\"a\"", Value "false")]
                    ]
                )
              ]
          )
        ]
    ),
    ( "[ 10.000000 , null, { \"ja\tck\": [1, -3, true, {\"a\" : false}]} ]",
      Array
        [ Value "10.000000",
          Value "null",
          Object
            [ ( "\"ja\tck\"",
                Array
                  [ Value "1",
                    Value "-3",
                    Value "true",
                    Object [("\"a\"", Value "false")]
                  ]
              )
            ]
        ]
    ),
    ("\\u001b\\u007f", Value "\\u001b\\u007f"),
    ("\"^[^@]+@[^@.]+\\.[^@]+$\"", Value "\"^[^@]+@[^@.]+\\.[^@]+$\"")
  ]

formatterTests :: [(ByteString, ByteString)]
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
