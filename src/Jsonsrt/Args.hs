{-# LANGUAGE DeriveGeneric #-}

module Jsonsrt.Args
  ( Args,
    parse,
    version,
    file,
    sortByName,
    sortByValue,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text)
import Options.Generic
  ( Generic,
    ParseRecord,
    getRecord,
    lispCaseModifiers,
    parseRecord,
    parseRecordWithModifiers,
  )

data Args = Args
  { version :: Bool,
    file :: Maybe FilePath,
    sortByName :: Bool,
    sortByValue :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

parse :: MonadIO m => m Args
parse = getRecord ""
