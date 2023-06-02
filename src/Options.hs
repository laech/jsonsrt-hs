{-# LANGUAGE DeriveGeneric #-}

module Options
  ( Options (..),
  )
where

import Data.Text.Lazy (Text)
import Options.Generic
  ( Generic,
    ParseRecord,
    lispCaseModifiers,
    parseRecord,
    parseRecordWithModifiers,
  )

data Options = Options
  { sortByName :: Bool,
    sortByValue :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
