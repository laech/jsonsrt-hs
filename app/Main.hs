module Main (main) where

import Data.ByteString.Lazy qualified as LBS
import Data.Jsonish qualified as Jsonish
import Data.Jsonish.Format qualified as Jsonish

main :: IO ()
main =
  LBS.getContents
    >>= either print (LBS.putStr . Jsonish.format) . Jsonish.parse
