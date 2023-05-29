module Main (main) where

import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Jsonish qualified as Jsonish
import Data.Jsonish.Format qualified as Jsonish

main :: IO ()
main =
  ByteString.getContents
    >>= either print (ByteString.putStrLn . Jsonish.format) . Jsonish.parse
