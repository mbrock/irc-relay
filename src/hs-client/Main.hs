module Main where

import Network
import Control.Monad
import Control.Concurrent.Chan

import Graphics.Vty.Widgets.All

import UserInterface
import Connection
import Channels

import System.Environment

main :: IO ()
main = withSocketsDo $ do
  
  -- addr and port to the relay
  xs <- getArgs
  [addr,port] <- case xs of
    []    -> putStrLn "Using default localhost:1339" >> return ["localhost","1339"]
    [_,_] -> return xs 
  
  chans <- newChannels
  
  initRelayConnection chans addr port
  
  putStrLn "Everything initialized - staring the UI"

  st <- initUI chans  
  
  -- Enter the event loop.
  runUi (uis st) (ctxt st) 

  