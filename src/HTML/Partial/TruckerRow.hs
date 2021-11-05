module HTML.Partial.TruckerRow where

import Lucid.Html5
import Lucid.HTMX

import App.Data (Trucker(..), TruckerFields(..))
import Lucid.Base (ToHtml)

import qualified App.Common as Common
import qualified HTML.Snippet as Snippet
import qualified Lucid.Base as Lucid
import qualified App.Tailwind as Tailwind

newtype TruckerRow = TruckerRow Trucker

instance ToHtml TruckerRow where
    toHtml (TruckerRow trucker) = do
        let rowId = "trucker-row-" <> Common.showT (truckerID trucker)

        tr_ [id_ rowId] $ do
            td_ [Tailwind.tableCell_ ""] $ Lucid.toHtml $ truckerID trucker
            td_ [Tailwind.tableCell_ ""] $ Lucid.toHtml $ truckerName trucker

            let fields = truckerFields trucker

            Snippet.toggle_ True $ truckerFieldsRR2 fields
            Snippet.toggle_ True $ truckerFieldsTHE fields
            Snippet.toggle_ True $ truckerFieldsREG1 fields
            Snippet.toggle_ True $ truckerFieldsREG2 fields
            Snippet.toggle_ True $ truckerFieldsMILES fields
            Snippet.toggle_ True $ truckerFieldsPTP fields
            Snippet.toggle_ True $ truckerFieldsBLU fields
            Snippet.toggle_ True $ truckerFieldsTRG fields

            td_ [Tailwind.tableCell_ ""] $ do
                span_ [class_ "flex flex-row justify-center align-middle"] $ do
                    button_
                        [ Tailwind.button_ "mr-2 bg-purple-400"
                        -- , hxGetSafe_ $ getContactFormLink cID
                        , hxTarget_ $ "#" <> rowId
                        , hxSwap_ "outerHTML"
                        ]
                        "Edit"
                    button_
                        [ Tailwind.button_ "bg-red-400"
                        -- , hxDeleteSafe_ $ deleteContactLink cID
                        , hxConfirm_ "Are you sure?"
                        , hxTarget_ $ "#" <> rowId
                        , hxSwap_ "outerHTML"
                        ]
                        "Delete"
    toHtmlRaw = Lucid.toHtml
