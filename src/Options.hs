{-# LANGUAGE DeriveGeneric #-}

module Options
  ( Options (..),
  )
where

import Options.Generic
  ( Generic,
    ParseRecord,
    lispCaseModifiers,
    parseRecord,
    parseRecordWithModifiers,
  )

newtype Options = Options
  { sortByName :: Bool
  }
  deriving (Show, Eq, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
