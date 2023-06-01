module Main (main) where

import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Jsonish (format, parse)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  ByteString.getContents >>= \content ->
    case parse content of
      Left err -> hPutStrLn stderr err >> exitFailure
      Right val -> ByteString.putStrLn (format val)
