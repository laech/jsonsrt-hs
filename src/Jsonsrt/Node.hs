{-# LANGUAGE StrictData #-}

module Jsonsrt.Node (Node (..)) where

import Data.Text.Lazy (Text)

data Node
  = Value Text
  | Array [Node]
  | Object [(Text, Node)]
  deriving (Show, Eq)
