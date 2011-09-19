-- Based on vty-ui's example ListTest
{-# LANGUAGE RecordWildCards #-}
module UserInterface where

import System.Exit ( exitSuccess )
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Control.Monad
import Control.Concurrent 
import Data.IORef

import Channels

data AppElements =
    AppElements { messageList      :: Widget (List String FormattedText)
                , theBody          :: Widget FormattedText
                , titleBar         :: Widget FormattedText
                , bottomBar        :: Widget FormattedText
                , inputRow         :: Widget Edit
                , messageListLimit :: Widget (VLimit (List String FormattedText))
                , ctxt             :: RenderContext
                , uis              :: Collection
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
  
  let cx = defaultContext { normalAttr = normAttr
                          , focusAttr = focAttr
                          }

  return $ AppElements { messageList      = ml
                       , theBody          = b
                       , titleBar         = tb
                       , bottomBar        = bb
                       , inputRow         = e
                       , messageListLimit = ll
                       , ctxt             = cx
                       , uis              = c
                       }

updateText :: Widget FormattedText -> String -> IO ()
updateText ft t = setText ft ("[" ++ t ++ "]")

initUI :: Channels -> IO AppElements
initUI (Channels toUI fromUI) = do
  
  st <- mkAppElements
  ui <- buildUi st
  fg <- newFocusGroup
  showMainUI <- addToCollection (uis st) ui fg
  addToFocusGroup fg (inputRow st)
  
  let addToMessageList s = do
        addToList (messageList st) s =<< plainText s
        scrollDown (messageList st)

  inputRow st `onActivate` \e -> do
         s <- getEditText e
         writeChan fromUI s
         addToMessageList ("echo : " ++ s)
         when (take 1 s == "q") (exitSuccess)
         setEditText e ""
         
  setEditText (inputRow st) ""
  focus (inputRow st)
  
  forkIO $ forever $ do 
    msg <- readChan toUI
    schedule $ addToMessageList ("relay : " ++ msg)

  updateText (titleBar st)  "titleBar"
  updateText (bottomBar st) "bottomBar"
      
  return st

