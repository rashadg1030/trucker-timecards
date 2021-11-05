module App.Common where

import Data.Text (Text)

import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text as Text

showT :: Show a => a -> Text
showT = Text.pack . show

showB :: Show a => a -> ByteStringLazy.ByteString
showB = ByteStringLazy.fromStrict . ByteStringChar8.pack . show

readT :: Read a => Text -> a
readT = read . Text.unpack
