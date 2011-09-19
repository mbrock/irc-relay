{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Exit ( exitSuccess )
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Control.Monad

data AppElements =
    AppElements { messageList :: Widget (List String FormattedText)
                , theBody :: Widget FormattedText
                , titleBar :: Widget FormattedText
                , bottomBar :: Widget FormattedText
                , inputRow :: Widget Edit
                , messageListLimit :: Widget (VLimit (List String FormattedText))
                , uis :: Collection
                }

-- Visual attributes.
titleAttr = bright_white `on` blue
editAttr = white `on` black
focAttr = white `on` black
normAttr = white `on` black
selAttr = white `on` black

buildUi (AppElements{..}) = do
           (return titleBar <++> hline)
      <-->  return messageList
      <--> (return bottomBar <++> hline)
      <-->  return inputRow
  where hline = hBorder >>= withBorderAttribute titleAttr

-- Construct the application state using the message map.
mkAppElements :: IO AppElements
mkAppElements = do
  ml <- newStringList selAttr []
  b  <- textWidget wrap ""
  tb <- plainText "" >>= withNormalAttribute titleAttr
  bb <- plainText "[]" >>= withNormalAttribute titleAttr
  e  <- editWidget
  ll <- vLimit 5 ml

  c <- newCollection

  return $ AppElements { messageList      = ml
                       , theBody          = b
                       , titleBar         = tb
                       , bottomBar        = bb
                       , inputRow         = e
                       , messageListLimit = ll
                       , uis              = c
                       }


updateFooterNums :: AppElements -> Widget (List a b) -> IO ()
updateFooterNums st w = do
  result <- getSelected w
  sz <- getListSize w
  let msg = case result of
              Nothing -> "--/--"
              Just (i, _) ->
                  "-" ++ (show $ i + 1) ++ "/" ++
                          (show sz) ++ "-"
  setText (bottomBar st) msg


updateText :: Widget FormattedText -> String -> IO ()
updateText ft t = setText ft ("[" ++ t ++ "]")

main :: IO ()
main = do
  
  st <- mkAppElements
  ui <- buildUi st
  fg <- newFocusGroup
  showMainUI <- addToCollection (uis st) ui fg
  addToFocusGroup fg (inputRow st)

  inputRow st `onActivate` \e -> do
         s <- getEditText e
         addToList (messageList st) s =<< plainText s
         scrollDown (messageList st)
         when (take 1 s == "q") (exitSuccess)
         setEditText e ""
         
  setEditText (inputRow st) ""
  focus (inputRow st)
  
  updateText (titleBar st)  "Topic : irc-relay rocks! github.com/mbrock/irc-relay"
  updateText (bottomBar st) "connected to irc.freenode.net"

  -- Enter the event loop.
  runUi (uis st) $ defaultContext { normalAttr = normAttr
                                  , focusAttr = focAttr
                                  }
