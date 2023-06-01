{-# LANGUAGE DeriveGeneric #-}

module Options
  ( Options (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import Options.Generic
  ( Generic,
    ParseRecord,
    lispCaseModifiers,
    parseRecord,
    parseRecordWithModifiers,
  )

data Options = Options
  { sortByName :: Bool,
    sortByValue :: Maybe ByteString
  }
  deriving (Show, Eq, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
