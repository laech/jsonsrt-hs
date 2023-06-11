{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Version (showVersion)
import Jsonsrt.App (run)
import Jsonsrt.Args qualified as Args
import Paths_jsonsrt (version)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs >>= Args.parse
  if Args.version args
    then putStrLn . showVersion $ version
    else
      run args >>= \case
        Left err -> hPutStrLn stderr err >> exitFailure
        Right _ -> pure ()
