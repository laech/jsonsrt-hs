{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.JsonishSpec (spec) where

import Control.Monad (forM_)
import Data.Jsonish (Jsonish (..), format, parse, sortByName, sortByValue)
import Data.String.Interpolate (i)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

escapeNewline :: Text -> String
escapeNewline = Text.unpack . Text.replace "\n" "\\n"

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
    forM_ formatterTests $ \(input, expected) ->
      it ("can pretty print: `" ++ escapeNewline input ++ "`") $
        either
          expectationFailure
          (`shouldBe` expected)
          (format <$> parse input)

  describe "sortByName" $ do
    forM_ sortByNameTests $ \(input, expected) ->
      it ("can sort this by name: " ++ show input) $
        sortByName input `shouldBe` expected

  describe "sortByValue" $ do
    forM_ sortByValueTests $ \(name, input, expected) ->
      it ("can sort this by value: " ++ show input) $
        sortByValue name input `shouldBe` expected

parserTests :: [(Text, Jsonish)]
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

sortByNameTests :: [(Jsonish, Jsonish)]
sortByNameTests =
  [ (Value "1", Value "1"),
    (Object [], Object []),
    (Object [("1", Value "a")], Object [("1", Value "a")]),
    (Object [("1", Value "a"), ("2", Value "b")], Object [("1", Value "a"), ("2", Value "b")]),
    (Object [("2", Value "b"), ("1", Value "a")], Object [("1", Value "a"), ("2", Value "b")]),
    ( Object
        [ ("2", Value "b"),
          ("1", Value "a"),
          ("3", Object [("1", Value "one"), ("0", Value "zero")])
        ],
      Object
        [ ("1", Value "a"),
          ("2", Value "b"),
          ("3", Object [("0", Value "zero"), ("1", Value "one")])
        ]
    ),
    ( Object
        [ ("2", Value "b"),
          ("1", Value "a"),
          ("3", Array [Object [("1", Value "one"), ("0", Value "zero")]])
        ],
      Object
        [ ("1", Value "a"),
          ("2", Value "b"),
          ("3", Array [Object [("0", Value "zero"), ("1", Value "one")]])
        ]
    ),
    (Array [], Array []),
    ( Array
        [ Object
            [ ("1", Value "one"),
              ("0", Value "zero")
            ]
        ],
      Array
        [ Object
            [ ("0", Value "zero"),
              ("1", Value "one")
            ]
        ]
    ),
    ( Array
        [ Object
            [ ("1", Value "one"),
              ("0", Array [Object [("y", Value "yy"), ("x", Value "xx")]])
            ]
        ],
      Array
        [ Object
            [ ("0", Array [Object [("x", Value "xx"), ("y", Value "yy")]]),
              ("1", Value "one")
            ]
        ]
    )
  ]

sortByValueTests :: [(Text, Jsonish, Jsonish)]
sortByValueTests =
  [ ("", Value "1", Value "1"),
    ("", Object [], Object []),
    ("", Array [], Array []),
    ( "name",
      Array
        [ Object [("\"name\"", Value "1")],
          Object [("\"name\"", Value "2")]
        ],
      Array
        [ Object [("\"name\"", Value "1")],
          Object [("\"name\"", Value "2")]
        ]
    ),
    ( "name",
      Array
        [ Object [("\"name\"", Value "2")],
          Object [("\"name\"", Value "1")]
        ],
      Array
        [ Object [("\"name\"", Value "1")],
          Object [("\"name\"", Value "2")]
        ]
    ),
    ( "name",
      Object
        [ ( "\"name\"",
            Array
              [ Object [("\"name\"", Value "2")],
                Object [("\"name\"", Value "1")]
              ]
          )
        ],
      Object
        [ ( "\"name\"",
            Array
              [ Object [("\"name\"", Value "1")],
                Object [("\"name\"", Value "2")]
              ]
          )
        ]
    ),
    ( "a",
      Array
        [ Object [("\"a\"", Value "1")],
          Object [("\"a\"", Value "2")],
          Object [("\"a\"", Value "0")]
        ],
      Array
        [ Object [("\"a\"", Value "0")],
          Object [("\"a\"", Value "1")],
          Object [("\"a\"", Value "2")]
        ]
    ),
    ( "a",
      Array
        [ Object [("\"a\"", Value "1")],
          Object [("\"a\"", Value "0")],
          Object
            [ ( "\"b\"",
                Array
                  [ Object [("\"a\"", Value "2")],
                    Object [("\"a\"", Value "1")]
                  ]
              )
            ]
        ],
      Array
        [ Object [("\"a\"", Value "0")],
          Object [("\"a\"", Value "1")],
          Object
            [ ( "\"b\"",
                Array
                  [ Object [("\"a\"", Value "1")],
                    Object [("\"a\"", Value "2")]
                  ]
              )
            ]
        ]
    )
  ]
