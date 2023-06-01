{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Jsonish qualified as Jsonish
import Options (Options (..))
import Options.Generic (getRecord)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  options :: Options <- getRecord ""
  print options
  content <- ByteString.getContents
  case Jsonish.parse content of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right val -> do
      print val
      let x1 = (if options.sortByName then Jsonish.sortByName else id) val
          x2 = maybe id Jsonish.sortByValue (options.sortByValue) x1
       in ByteString.putStrLn (Jsonish.format x2)
