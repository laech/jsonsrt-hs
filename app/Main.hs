module Main (main) where

import Data.Jsonish qualified as Jsonish
import Data.Text.IO qualified as Text

main :: IO ()
main = Text.getContents >>= either print Text.putStrLn . Jsonish.format
