{-# LANGUAGE OverloadedRecordDot #-}

module Jsonsrt.ArgsSpec (spec) where

import Jsonsrt.Args (Args (..), parse)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parse" $ do
    it "has expected defaults" $ do
      args <- parse []
      args
        `shouldBe` Args
          { version = False,
            sortByName = False,
            sortByValue = Nothing,
            file = Nothing
          }

    it "can parse version flag" $ do
      args <- parse ["--version"]
      args.version `shouldBe` True

    it "can parse sort by name flag" $ do
      args <- parse ["--sort-by-name"]
      args.sortByName `shouldBe` True

    it "can parse sort by value option" $ do
      args <- parse ["--sort-by-value", "bob"]
      args.sortByValue `shouldBe` Just "bob"

    it "can parse file argument" $ do
      args <- parse ["test.json"]
      args.file `shouldBe` Just "test.json"
