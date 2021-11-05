module HTML.Page.DataEntry where

import Lucid.Html5

import Lucid.Base (ToHtml)
import App.Data (Trucker(..))

import qualified App.Common as Common
import qualified Lucid.Base as Lucid
import qualified HTML.Snippet as Snippet

newtype DataEntry = DataEntry (Maybe Trucker)

instance ToHtml DataEntry where
    toHtml (DataEntry mbTrucker) = Snippet.baseTemplate "Trucker Timecards" $ do
        Snippet.navWithSearch
        div_ [class_ "m-10"] $ do
            case mbTrucker of
                Nothing -> do
                    h1_ [class_ "text-xl font-semibold"] "Use the search bar above to select a trucker for data entry."
                Just trucker -> do
                    h1_ [] $ Lucid.toHtml $ "Trucker " <> Common.showT (truckerName trucker) <> " found"
    toHtmlRaw = Lucid.toHtml
