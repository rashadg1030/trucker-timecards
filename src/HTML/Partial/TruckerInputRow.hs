module HTML.Partial.TruckerInputRow where

import Lucid.Html5
import Lucid.HTMX

import Lucid.Base (ToHtml)
import App.Data (Trucker(..), TruckerFields(..))

import qualified App.Common as Common
import qualified App.Tailwind as Tailwind
import qualified HTML.Snippet as Snippet
import qualified Lucid.Base as Lucid


newtype TruckerInputRow = TruckerInputRow (Maybe Trucker)

instance ToHtml TruckerInputRow where
    toHtml (TruckerInputRow mbTrucker) =
        case mbTrucker of
            Nothing -> do
                tr_ [id_ ""] $ do
                    td_ [Tailwind.tableCell_ ""] ""
                    td_ [Tailwind.tableCell_ ""] $
                        input_
                            [ class_ "rounded-md px-2 border-2"
                            , type_ "text"
                            , name_ ""
                            ]

                    Snippet.toggle_ False False
                    Snippet.toggle_ False False
                    Snippet.toggle_ False False
                    Snippet.toggle_ False False
                    Snippet.toggle_ False False
                    Snippet.toggle_ False False
                    Snippet.toggle_ False False
                    Snippet.toggle_ False False

                    td_ [Tailwind.tableCell_ ""] $ do
                        span_ [class_ "flex flex-row justify-center align-middle"] $ do
                            button_
                                [ Tailwind.button_ "mr-2 bg-purple-400"
                                -- , hxGetSafe_ $ getContactFormLink cID
                                -- , hxTarget_ $ "#" <> rowId
                                , hxSwap_ "outerHTML"
                                ]
                                "Edit"
                            button_
                                [ Tailwind.button_ "bg-red-400"
                                -- , hxDeleteSafe_ $ deleteContactLink cID
                                , hxConfirm_ "Are you sure?"
                                -- , hxTarget_ $ "#" <> rowId
                                , hxSwap_ "outerHTML"
                                ]
                                "Delete"
            Just trucker -> do
                tr_ [id_ ""] $ do
                    td_ [Tailwind.tableCell_ ""] $
                        input_
                            [ class_ "rounded-md px-2 border-2"
                            , type_ "text"
                            , name_ ""
                            , value_ $ Common.showT $ truckerID trucker
                            ]
                    td_ [Tailwind.tableCell_ ""] $
                        input_
                            [ class_ "rounded-md px-2 border-2"
                            , type_ "text"
                            , name_ ""
                            , value_ $ Common.showT $ truckerName trucker
                            ]
                    
                    let fields = truckerFields trucker

                    Snippet.toggle_ False $ truckerFieldsRR2 fields
                    Snippet.toggle_ False $ truckerFieldsTHE fields
                    Snippet.toggle_ False $ truckerFieldsREG1 fields
                    Snippet.toggle_ False $ truckerFieldsREG2 fields
                    Snippet.toggle_ False $ truckerFieldsMILES fields
                    Snippet.toggle_ False $ truckerFieldsPTP fields
                    Snippet.toggle_ False $ truckerFieldsBLU fields
                    Snippet.toggle_ False $ truckerFieldsTRG fields

                    td_ [Tailwind.tableCell_ ""] $ do
                        span_ [class_ "flex flex-row justify-center align-middle"] $ do
                            button_
                                [ Tailwind.button_ "mr-2 bg-green-400"
                                -- , hxGetSafe_ $ getContactFormLink cID
                                -- , hxTarget_ $ "#" <> rowId
                                , hxSwap_ "outerHTML"
                                ]
                                "Save"
                            button_
                                [ Tailwind.button_ "bg-red-400"
                                -- , hxDeleteSafe_ $ deleteContactLink cID
                                , hxConfirm_ "Are you sure?"
                                -- , hxTarget_ $ "#" <> rowId
                                , hxSwap_ "outerHTML"
                                ]
                                "Cancel"
    toHtmlRaw = Lucid.toHtml
