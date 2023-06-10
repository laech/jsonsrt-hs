{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Text.Lazy.IO qualified as Text
import Data.Version (showVersion)
import Jsonsrt.Format (format)
import Jsonsrt.Options (Options (..))
import Jsonsrt.Parse (parse)
import Jsonsrt.Sort (sortByName, sortByValue)
import Options.Generic (getRecord)
import Paths_jsonsrt qualified as App
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  options :: Options <- getRecord ""
  if optVersion options
    then putStrLn . showVersion $ App.version
    else do
      content <- Text.getContents
      case parse content of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right val -> do
          print val
          let x1 = (if optSortByName options then sortByName else id) val
              x2 = maybe id sortByValue (optSortByValue options) x1
           in Text.putStrLn (format x2)
