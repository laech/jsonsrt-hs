{-# LANGUAGE LambdaCase #-}

module Jsonsrt.Sort (sortByName, sortByValue) where

import Control.Arrow (second)
import Data.Foldable (find)
import Data.List (sortBy, sortOn)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Jsonsrt.Node (Node (..))

sortByName :: Node -> Node
sortByName = \case
  v@(Value _) -> v
  Object xs -> Object . sortOn (unquote . fst) . fmap (second sortByName) $ xs
  Array xs -> Array . fmap sortByName $ xs

sortByValue :: Text -> Node -> Node
sortByValue name = \case
  v@(Value _) -> v
  Object xs -> Object . fmap (second $ sortByValue name) $ xs
  Array xs -> Array . sortBy compareByValue . fmap (sortByValue name) $ xs
  where
    compareByValue a b = case (findValue a, findValue b) of
      (Just (Value x), Just (Value y)) -> compare (unquote x) (unquote y)
      _ -> EQ
    findValue = \case
      (Object xs) -> fmap snd . find ((("\"" <> name <> "\"") ==) . fst) $ xs
      _ -> Nothing

unquote :: Text -> Text
unquote str =
  if Text.compareLength str 1 == GT
    && "\"" `Text.isPrefixOf` str
    && "\"" `Text.isSuffixOf` str
    then Text.drop 1 . Text.dropEnd 1 $ str
    else str
