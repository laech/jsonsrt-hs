module Jsonsrt.SortSpec (spec) where

import Control.Monad (forM_)
import Data.Text.Lazy (Text)
import Jsonsrt.Node (Node (..))
import Jsonsrt.Sort (sortByName, sortByValue)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "sortByName" $ do
    forM_ sortByNameTests $ \(input, expected) ->
      it ("can sort this by name: " ++ show input) $
        sortByName input `shouldBe` expected

  describe "sortByValue" $ do
    forM_ sortByValueTests $ \(name, input, expected) ->
      it ("can sort this by value: " ++ show input) $
        sortByValue name input `shouldBe` expected

sortByNameTests :: [(Node, Node)]
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

sortByValueTests :: [(Text, Node, Node)]
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
