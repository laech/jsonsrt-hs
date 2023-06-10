{-# LANGUAGE LambdaCase #-}

module Jsonsrt.Sort (sortByName, sortByValue) where

import Control.Arrow (second)
import Data.Foldable (find)
import Data.List (sortBy, sortOn)
import Data.Text.Lazy (Text)
import Jsonsrt.Node (Node (..))

sortByName :: Node -> Node
sortByName = \case
  v@(Value _) -> v
  Object xs -> Object . sortOn fst . fmap (second sortByName) $ xs
  Array xs -> Array . fmap sortByName $ xs

sortByValue :: Text -> Node -> Node
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
