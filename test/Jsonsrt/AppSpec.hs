{-# LANGUAGE QuasiQuotes #-}

module Jsonsrt.AppSpec (spec) where

import Control.Exception (bracket)
import Data.String.Interpolate (i)
import Jsonsrt.App qualified as App
import Jsonsrt.Args qualified as Args
import System.Directory (removeFile)
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcess)
import Test.Hspec (Spec, describe, it, shouldBe)

withTempFile :: String -> (FilePath -> IO ()) -> IO ()
withTempFile content action = do
  bracket
    (openTempFile "." "tmp")
    ( \(path, handle) -> do
        hClose handle
        removeFile path
    )
    ( \(path, handle) -> do
        hPutStr handle content
        hClose handle
        action path
    )

spec :: Spec
spec = do
  describe "app" $ do
    it "can use stdin/stdout" $ do
      output <- readProcess "cabal" ["run", "-v0", "jsonsrt"] "{ }"
      output `shouldBe` "{}\n"

    it "can use file" $ withTempFile "{ }" $ \path -> do
      args <- Args.parse [path]
      App.run args >>= (`shouldBe` Right ())
      readFile path >>= (`shouldBe` "{}\n")

    it "can sort by name" $ withTempFile [i|{"1":0,"0":0}|] $ \path -> do
      args <- Args.parse ["--sort-by-name", path]
      App.run args >>= (`shouldBe` Right ())
      content <- readFile path
      content
        `shouldBe` [i|{
  "0": 0,
  "1": 0
}
|]

    it "can sort by value" $ withTempFile [i|[{"x":1},{"x":0}]|] $ \path -> do
      args <- Args.parse ["--sort-by-value", "x", path]
      App.run args >>= (`shouldBe` Right ())
      content <- readFile path
      content
        `shouldBe` [i|[
  {
    "x": 0
  },
  {
    "x": 1
  }
]
|]
