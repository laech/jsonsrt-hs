{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Text.Lazy.IO qualified as Text
import Data.Version (showVersion)
import Jsonish qualified
import Options (Options (..))
import Options.Generic (getRecord)
import Paths_jsonsrt qualified as App
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  options :: Options <- getRecord ""
  if options.version
    then putStrLn . showVersion $ App.version
    else do
      content <- Text.getContents
      case Jsonish.parse content of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right val -> do
          print val
          let x1 = (if options.sortByName then Jsonish.sortByName else id) val
              x2 = maybe id Jsonish.sortByValue (options.sortByValue) x1
           in Text.putStrLn (Jsonish.format x2)
