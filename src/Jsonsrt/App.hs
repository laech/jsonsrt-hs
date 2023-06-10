{-# LANGUAGE OverloadedRecordDot #-}

module Jsonsrt.App (run) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO qualified as Text
import Jsonsrt.Args (Args)
import Jsonsrt.Args qualified as Args
import Jsonsrt.Format (format)
import Jsonsrt.Parse (parse)
import Jsonsrt.Sort (sortByName, sortByValue)

run :: Args -> IO (Either String ())
run args =
  input args.file
    >>= mapM (output args.file)
      . fmap srt
      . parse
  where
    srt =
      format
        . maybe id sortByValue (Args.sortByValue args)
        . (if Args.sortByName args then sortByName else id)

input :: Maybe FilePath -> IO Text
input = maybe Text.getContents Text.readFile

output :: Maybe FilePath -> Text -> IO ()
output = do
  maybe Text.putStrLn writeFileLn
  where
    writeFileLn path text = Text.writeFile path (text <> "\n")
