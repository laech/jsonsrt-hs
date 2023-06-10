{-# LANGUAGE DeriveGeneric #-}

module Jsonsrt.Options
  ( Options (..),
    getOptions,
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

data Options = Options
  { optVersion :: Bool,
    optSortByName :: Bool,
    optSortByValue :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

getOptions :: MonadIO m => m Options
getOptions = getRecord ""
