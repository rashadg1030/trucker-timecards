{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Navbar where

import Lucid
import Lucid.Supplemental
import System.IO (stdout, hSetEncoding, utf8)
import Data.Text.Lazy.IO as L

-- Template for file: snippets/navbar-clean.html
template1 :: Monad m => HtmlT m ()
template1 = do
  nav_ [ class_ "bg-gray-800" ] $ do
    div_ [ class_ "max-w-7xl mx-auto px-2 sm:px-4 lg:px-8" ] $ div_ [ class_ "relative flex items-center justify-between h-16" ] $ do
      div_ [ class_ "flex items-center px-2 lg:px-0" ] $ do
        div_ [ class_ "flex-shrink-0" ] $ do
          img_ [ class_ "block lg:hidden h-8 w-auto", src_ "https://tailwindui.com/img/logos/workflow-mark-indigo-500.svg", alt_ "Workflow" ]
          img_ [ class_ "hidden lg:block h-8 w-auto", src_ "https://tailwindui.com/img/logos/workflow-logo-indigo-500-mark-white-text.svg", alt_ "Workflow" ]
        div_ [ class_ "hidden lg:block lg:ml-6" ] $ div_ [ class_ "flex space-x-4" ] $ do
          a_ [ href_ "#", class_ "bg-gray-900 text-white px-3 py-2 rounded-md text-sm font-medium" ] $ "Dashboard"
          a_ [ href_ "#", class_ "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium" ] $ "Team"
          a_ [ href_ "#", class_ "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium" ] $ "Projects"
          a_ [ href_ "#", class_ "text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium" ] $ "Calendar"
      div_ [ class_ "flex-1 flex justify-center px-2 lg:ml-6 lg:justify-end" ] $ div_ [ class_ "max-w-lg w-full lg:max-w-xs" ] $ do
        label_ [ for_ "search", class_ "sr-only" ] $ "Search"
        div_ [ class_ "relative" ] $ do
          div_ [ class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none" ] $ svg_ [ class_ "h-5 w-5 text-gray-400", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 20 20", ariaHidden_ "true" ] $ ""
          input_ [ id_ "search", name_ "search", class_ "block w-full pl-10 pr-3 py-2 border border-transparent rounded-md leading-5 bg-gray-700 text-gray-300 placeholder-gray-400 focus:outline-none focus:bg-white focus:border-white focus:ring-white focus:text-gray-900 sm:text-sm", placeholder_ "Search", type_ "search" ]
      div_ [ class_ "flex lg:hidden" ] $ button_ [ type_ "button", class_ "inline-flex items-center justify-center p-2 rounded-md text-gray-400 hover:text-white hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-white", ariaExpanded_ "false" ] $ do
        span_ [ class_ "sr-only" ] $ "Open main menu"
        svg_ [ class_ "block h-6 w-6", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24", ariaHidden_ "true" ] $ ""
        svg_ [ class_ "hidden h-6 w-6", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24", ariaHidden_ "true" ] $ ""
      div_ [ class_ "hidden lg:block lg:ml-4" ] $ div_ [ class_ "flex items-center" ] $ do
        button_ [ type_ "button", class_ "flex-shrink-0 bg-gray-800 p-1 rounded-full text-gray-400 hover:text-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white" ] $ do
          span_ [ class_ "sr-only" ] $ "View notifications"
          svg_ [ class_ "h-6 w-6", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24", ariaHidden_ "true" ] $ ""
        div_ [ class_ "ml-4 relative flex-shrink-0" ] $ do
          div_ $ button_ [ type_ "button", class_ "bg-gray-800 rounded-full flex text-sm text-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white", id_ "user-menu-button", ariaExpanded_ "false", ariaHaspopup_ "true" ] $ do
            span_ [ class_ "sr-only" ] $ "Open user menu"
            img_ [ class_ "h-8 w-8 rounded-full", src_ "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80", alt_ "" ]
          div_ [ class_ "origin-top-right absolute right-0 mt-2 w-48 rounded-md shadow-lg py-1 bg-white ring-1 ring-black ring-opacity-5 focus:outline-none", role_ "menu", ariaLabelledby_ "user-menu-button", tabindex_ "-1" ] $ do
            a_ [ href_ "#", class_ "block px-4 py-2 text-sm text-gray-700", role_ "menuitem", tabindex_ "-1", id_ "user-menu-item-0" ] $ "Your Profile"
            a_ [ href_ "#", class_ "block px-4 py-2 text-sm text-gray-700", role_ "menuitem", tabindex_ "-1", id_ "user-menu-item-1" ] $ "Settings"
            a_ [ href_ "#", class_ "block px-4 py-2 text-sm text-gray-700", role_ "menuitem", tabindex_ "-1", id_ "user-menu-item-2" ] $ "Sign out"
    div_ [ class_ "lg:hidden", id_ "mobile-menu" ] $ do
      div_ [ class_ "px-2 pt-2 pb-3 space-y-1" ] $ do
        a_ [ href_ "#", class_ "bg-gray-900 text-white block px-3 py-2 rounded-md text-base font-medium" ] $ "Dashboard"
        a_ [ href_ "#", class_ "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium" ] $ "Team"
        a_ [ href_ "#", class_ "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium" ] $ "Projects"
        a_ [ href_ "#", class_ "text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium" ] $ "Calendar"
      div_ [ class_ "pt-4 pb-3 border-t border-gray-700" ] $ do
        div_ [ class_ "flex items-center px-5" ] $ do
          div_ [ class_ "flex-shrink-0" ] $ img_ [ class_ "h-10 w-10 rounded-full", src_ "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80", alt_ "" ]
          div_ [ class_ "ml-3" ] $ do
            div_ [ class_ "text-base font-medium text-white" ] $ "Tom Cook"
            div_ [ class_ "text-sm font-medium text-gray-400" ] $ "tom@example.com"
          button_ [ type_ "button", class_ "ml-auto flex-shrink-0 bg-gray-800 p-1 rounded-full text-gray-400 hover:text-white focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-800 focus:ring-white" ] $ do
            span_ [ class_ "sr-only" ] $ "View notifications"
            svg_ [ class_ "h-6 w-6", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24", ariaHidden_ "true" ] $ ""
        div_ [ class_ "mt-3 px-2 space-y-1" ] $ do
          a_ [ href_ "#", class_ "block px-3 py-2 rounded-md text-base font-medium text-gray-400 hover:text-white hover:bg-gray-700" ] $ "Your\n                    Profile"
          a_ [ href_ "#", class_ "block px-3 py-2 rounded-md text-base font-medium text-gray-400 hover:text-white hover:bg-gray-700" ] $ "Settings"
          a_ [ href_ "#", class_ "block px-3 py-2 rounded-md text-base font-medium text-gray-400 hover:text-white hover:bg-gray-700" ] $ "Sign\n                    out"

