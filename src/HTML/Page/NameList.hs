module HTML.Page.NameList where

import Lucid.Html5

import Lucid.Base (ToHtml)
import HTML.Partial.TruckerInputRow (TruckerInputRow(..))
import HTML.Partial.TruckerRow (TruckerRow)
-- import App.Data (Trucker(..))

import qualified App.Tailwind as Tailwind
import qualified HTML.Snippet as Snippet
import qualified Lucid.Base as Lucid


data Type = Type

newtype NameList = NameList [TruckerRow]

instance ToHtml NameList where
    toHtml (NameList truckerRows) = do
        Snippet.nav
        div_ [class_ "mt-10 flex justify-center"] $ do
            table_ [class_ "table-auto rounded-lg"] $ do
                thead_ [] $
                    tr_ [] $ do
                        th_ [Tailwind.tableHeader_ ""] "ID"
                        th_ [Tailwind.tableHeader_ ""] "Name"
                        th_ [Tailwind.tableHeader_ ""] "RR2"
                        th_ [Tailwind.tableHeader_ ""] "THE"
                        th_ [Tailwind.tableHeader_ ""] "REG1"
                        th_ [Tailwind.tableHeader_ ""] "REG2"
                        th_ [Tailwind.tableHeader_ ""] "MILES"
                        th_ [Tailwind.tableHeader_ ""] "PTP"
                        th_ [Tailwind.tableHeader_ ""] "BLU"
                        th_ [Tailwind.tableHeader_ ""] "TRG"
                        th_ [Tailwind.tableHeader_ ""] "Actions"
                tbody_ $ do
                    Prelude.mapM_ Lucid.toHtml truckerRows
                    Lucid.toHtml $ TruckerInputRow Nothing
    toHtmlRaw = Lucid.toHtml
