module App.Tailwind where

import Lucid.Html5

import Data.Text (Text)
import Lucid.Base (Attribute)


button_ :: Text -> Attribute
button_ custom = class_ $
    "px-4 py-2 text-lg text-white rounded-md " <> custom

tableCell_ :: Text -> Attribute
tableCell_ custom = class_ $
    "border-4 items-center justify-center px-4 py-2 text-semibold text-lg text-center " <> custom

tableHeader_ :: Text -> Attribute
tableHeader_ = tableCell_
