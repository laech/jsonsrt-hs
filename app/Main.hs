module Main (main) where

import Data.Jsonish qualified as Jsonish
import Data.Jsonish.Format qualified as Jsonish
import Data.Text.Lazy.IO as Text

main :: IO ()
main =
  Text.getContents
    >>= either print (Text.putStrLn . Jsonish.format) . Jsonish.parse
