{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module HTML.Snippet where

import Lucid.Alpine
import Lucid.Html5
import Lucid.HTMX
import Lucid.Supplemental

import Lucid.Base (HtmlT)
import Data.Text (Text)

import qualified Lucid.Base as Lucid
import qualified App.Tailwind as Tailwind

-- import System.IO (stdout, hSetEncoding, utf8)
-- import Data.Text.Lazy.IO as L


noHtml :: Monad m => HtmlT m ()
noHtml = ""

baseTemplate :: Monad m => Text -> HtmlT m a -> HtmlT m a
baseTemplate title innerHtml = do
    doctype_

    html_ [lang_ "en"] $ do

        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

            title_ $ Lucid.toHtml title

            link_ [href_ "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css", rel_ "stylesheet"]
            script_ [src_ "https://unpkg.com/alpinejs@3.4.2/dist/cdn.min.js", defer_ ""] noHtml
            script_ [src_ "https://unpkg.com/htmx.org@1.5.0"] noHtml
            script_ [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js"] noHtml

        body_ [id_ "main-content"] innerHtml

toggle_ :: Monad m => Bool -> Bool -> HtmlT m ()
toggle_ isDisabled isToggled = td_ [Tailwind.tableCell_ ""] $
    button_
        -- TODO: Why do Lucid attributes render out of order?
        [ type_ "button"
        , xData_ $ if isToggled then Just "{ toggled: true }" else Just "{ toggled: false }"
        , class_ "relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        , role_ "switch"
        , if isDisabled then disabled_ "" else class_ ""
        , xOn_ "click" "toggled = !toggled"
        , xBind_ "class" "{ 'bg-indigo-600': toggled, 'bg-gray-200': !toggled }"
        ] $ do
            span_ [class_ "sr-only"] "Use setting"
            span_ 
                [ class_ "pointer-events-none relative inline-block h-5 w-5 rounded-full bg-white shadow ring-0 transition ease-in-out duration-200"
                , xBind_ "class" "{ 'translate-x-5': toggled, 'translate-x-0': !toggled }"
                ] $ do
                    span_
                        [class_ "absolute inset-0 h-full w-full flex items-center justify-center transition-opacity opacity-0 ease-out duration-100"
                        , xBind_ "class" "{ 'opacity-0 ease-out duration-100': toggled, 'opacity-100 ease-in duration-200': !toggled }"
                        ] $
                            svg_ [class_ "h-3 w-3 text-gray-400", fill_ "none", viewBox_ "0 0 12 12"] $
                                path_ [d_ "M4 8l2-2m0 0l2-2M6 6L4 4m2 2l2 2", stroke_ "currentColor", strokeWidth_ "2", strokeLinecap_ "round", strokeLinejoin_ "round"]
                    span_
                        [class_ "absolute inset-0 h-full w-full flex items-center justify-center transition-opacity opacity-100 ease-in duration-100"
                        , xBind_ "class" "{ 'opacity-100 ease-in duration-200': toggled, 'opacity-0 ease-out duration-100': !toggled }"
                        ] $
                            svg_ [class_ "h-3 w-3 text-indigo-600", fill_ "currentColor", viewBox_ "0 0 12 12"] $
                                path_ [d_ "M3.707 5.293a1 1 0 00-1.414 1.414l1.414-1.414zM5 8l-.707.707a1 1 0 001.414 0L5 8zm4.707-3.293a1 1 0 00-1.414-1.414l1.414 1.414zm-7.414 2l2 2 1.414-1.414-2-2-1.414 1.414zm3.414 2l4-4-1.414-1.414-4 4 1.414 1.414z"]
    {-
        <button type="button" class="bg-gray-200 relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500" role="switch" aria-checked="false">
            <span class="sr-only">Use setting</span>
            <!-- Enabled: "translate-x-5", Not Enabled: "translate-x-0" -->
            <span class="translate-x-0 pointer-events-none relative inline-block h-5 w-5 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200">
                <!-- Enabled: "opacity-0 ease-out duration-100", Not Enabled: "opacity-100 ease-in duration-200" -->
                <span class="opacity-100 ease-in duration-200 absolute inset-0 h-full w-full flex items-center justify-center transition-opacity" aria-hidden="true">
                    <svg class="h-3 w-3 text-gray-400" fill="none" viewBox="0 0 12 12">
                        <path d="M4 8l2-2m0 0l2-2M6 6L4 4m2 2l2 2" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                    </svg>
                </span>
                <!-- Enabled: "opacity-100 ease-in duration-200", Not Enabled: "opacity-0 ease-out duration-100" -->
                <span class="opacity-0 ease-out duration-100 absolute inset-0 h-full w-full flex items-center justify-center transition-opacity" aria-hidden="true">
                    <svg class="h-3 w-3 text-indigo-600" fill="currentColor" viewBox="0 0 12 12">
                        <path d="M3.707 5.293a1 1 0 00-1.414 1.414l1.414-1.414zM5 8l-.707.707a1 1 0 001.414 0L5 8zm4.707-3.293a1 1 0 00-1.414-1.414l1.414 1.414zm-7.414 2l2 2 1.414-1.414-2-2-1.414 1.414zm3.414 2l4-4-1.414-1.414-4 4 1.414 1.414z" />
                    </svg>
                </span>
            </span>
        </button>
    -}

-- Template for file: snippets/navbar-clean.html
navWithSearch :: Monad m => HtmlT m ()
navWithSearch = do
  nav_ [ class_ "bg-gray-800" ] $ do
    div_ [ class_ "max-w-7xl mx-auto px-2 sm:px-4 lg:px-8" ] $ div_ [ class_ "relative flex items-center justify-between h-16" ] $ do
      div_ [ class_ "flex items-center px-2 lg:px-0" ] $ do
        div_ [ class_ "flex-shrink-0" ] $ do
          h1_ [ class_ "text-2xl font-bold tracking-wider text-gray-200"] "Trucker Timecards"
        div_ [ class_ "hidden lg:block lg:ml-6" ] $ div_ [ class_ "flex space-x-4" ] $ do
          a_ [ href_ "#", class_ "bg-gray-900 text-white px-3 py-2 rounded-md text-sm font-medium" ] $ "Data Entry"
          a_
            [ href_ "#"
            , class_ "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
            , hxGet_ "/trucker"
            , hxSwap_ "innerHTML"
            , hxTarget_ "#main-content"
            ] $ "Name List"
      div_ [ class_ "flex-1 flex justify-center px-2 lg:justify-start lg:ml-6" ] $ div_ [ class_ "max-w-lg w-full lg:max-w-xs" ] $ do
        label_ [ for_ "search", class_ "sr-only" ] $ "Search"
        div_ [ class_ "relative" ] $ do
          div_ [ class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none" ] $ svg_ [ class_ "h-5 w-5 text-gray-400", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 20 20", ariaHidden_ "true" ] $ do
            svg_ [class_ "h-5 w-5 text-gray-400", viewBox_ "0 0 20 20", fill_ "currentColor"] $
              path_ [fillRule_ "evenodd", d_ "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z", clipRule_ "evenodd"]
          input_ [ id_ "search", name_ "search", class_ "block w-full pl-10 pr-3 py-2 border border-transparent rounded-md leading-5 bg-gray-700 text-gray-300 placeholder-gray-400 focus:outline-none focus:bg-white focus:border-white focus:ring-white focus:text-gray-900 sm:text-sm", placeholder_ "Search Name", type_ "search" ]
    div_ [ class_ "lg:hidden", id_ "mobile-menu" ] $ do
      div_ [ class_ "px-2 pt-2 pb-3 space-y-1" ] $ do
        a_ [ href_ "#", class_ "bg-gray-900 text-white block px-3 py-2 rounded-md text-base font-medium" ] $ "Data Entry"
        a_ [ href_ "#", class_ "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium" ] $ "Name List"

nav :: Monad m => HtmlT m ()
nav = do
  nav_ [ class_ "bg-gray-800" ] $ do
    div_ [ class_ "max-w-7xl mx-auto px-2 sm:px-4 lg:px-8" ] $ div_ [ class_ "relative flex items-center justify-between h-16" ] $ do
      div_ [ class_ "flex items-center px-2 lg:px-0" ] $ do
        div_ [ class_ "flex-shrink-0" ] $ do
          h1_ [ class_ "text-2xl font-bold tracking-wider text-gray-200"] "Trucker Timecards"
        div_ [ class_ "hidden lg:block lg:ml-6" ] $ div_ [ class_ "flex space-x-4" ] $ do
          a_
            [ href_ "#"
            , class_ "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
            , hxGet_ "/"
            , hxSwap_ "innerHTML"
            , hxTarget_ "#main-content"
            ] $ "Data Entry"
          a_ [ href_ "#", class_ "bg-gray-900 text-white px-3 py-2 rounded-md text-sm font-medium" ] $ "Name List"
      div_ [ class_ "flex-1 flex justify-center px-2 lg:justify-start lg:ml-6" ] $ div_ [ class_ "max-w-lg w-full lg:max-w-xs" ] $ do
        label_ [ for_ "search", class_ "sr-only" ] $ "Search"
        div_ [ class_ "relative" ] $ do
          div_ [ class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none" ] $ svg_ [ class_ "h-5 w-5 text-gray-400", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 20 20", ariaHidden_ "true" ] $ do
            svg_ [class_ "h-5 w-5 text-gray-400", viewBox_ "0 0 20 20", fill_ "currentColor"] $
              path_ [fillRule_ "evenodd", d_ "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z", clipRule_ "evenodd"]
          input_ [ id_ "search", name_ "search", class_ "block w-full pl-10 pr-3 py-2 border border-transparent rounded-md leading-5 bg-gray-700 text-gray-300 placeholder-gray-400 focus:outline-none focus:bg-white focus:border-white focus:ring-white focus:text-gray-900 sm:text-sm", placeholder_ "Search Name", type_ "search" ]
    div_ [ class_ "lg:hidden", id_ "mobile-menu" ] $ do
      div_ [ class_ "px-2 pt-2 pb-3 space-y-1" ] $ do
        a_ [ href_ "#", class_ "bg-gray-900 text-white block px-3 py-2 rounded-md text-base font-medium" ] $ "Data Entry"
        a_ [ href_ "#", class_ "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium" ] $ "Name List"

{-
<svg class="h-5 w-5 text-gray-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
    <path fill-rule="evenodd"
        d="M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z"
        clip-rule="evenodd" />
</svg>
-}
