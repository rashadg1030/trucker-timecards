{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Navbar where

import Lucid
import Lucid.HTMX
import Lucid.Supplemental
import System.IO (stdout, hSetEncoding, utf8)
import Data.Text.Lazy.IO as L

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
