module Jsonsrt.Args
  ( Args (..),
    parse,
  )
where

import Data.Text.Lazy (Text)
import Options.Applicative
  ( Parser,
    argument,
    defaultPrefs,
    execParserPure,
    fullDesc,
    handleParseResult,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    progDesc,
    str,
    strOption,
    switch,
    (<**>),
  )

data Args = Args
  { version :: Bool,
    sortByName :: Bool,
    sortByValue :: Maybe Text,
    file :: Maybe FilePath
  }
  deriving (Show, Eq)

args :: Parser Args
args =
  Args
    <$> switch
      ( long "version"
          <> help "Output version information and exit"
      )
    <*> switch
      ( long "sort-by-name"
          <> help "Sort objects by key names"
      )
    <*> optional
      ( strOption
          ( long "sort-by-value"
              <> metavar "KEY"
              <> help "Sort object arrays by comparing the values of KEY"
          )
      )
    <*> optional
      ( argument
          str
          ( metavar "FILE"
              <> help "File to process, otherwise uses stdin/stdout"
          )
      )

parse :: [String] -> IO Args
parse =
  handleParseResult
    . execParserPure
      defaultPrefs
      ( info
          (args <**> helper)
          ( fullDesc
              <> progDesc "Sort JSON contents"
          )
      )
