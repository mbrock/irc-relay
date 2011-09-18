{-# LANGUAGE ViewPatterns #-}
module Main where

import Network
import Control.Monad 
import Control.Concurrent (threadDelay)

import System.IO 
import System.Environment

import Data.Maybe (maybeToList)
import Text.JSON

main = withSocketsDo $ do
    -- Connect to the relay
    [addr,port] <- getArgs
    h <- connectTo addr (PortNumber (fromIntegral (read port)))
    hSetBuffering h LineBuffering

    -- Send connect messages, USER and NICK
    let ircs = "irc.freenode.net"
        cmsg = encode (connectMessage ircs "6667")
        umsg = encode (userMessage "irc-relay-hs-client" ircs)
        nmsg = encode (nickMessage "irc-relay-hs-client" ircs)        
        sleep = threadDelay . (* 10^6)
    sleep 1
    hPutStrLn h cmsg
    sleep 1
    hPutStrLn h umsg
    sleep 1
    hPutStrLn h nmsg
    
    -- Print the responses
    hGetContents h >>= putStrLn

connectMessage :: String -> String -> JSObject String
connectMessage hostname port = toJSObject [ ("command","connect")
                                          , ("hostname",hostname)
                                          , ("port",port)
                                          ]

str :: String -> JSValue
str = JSString . toJSString

obj :: [(String,JSValue)] -> JSValue
obj = JSObject . toJSObject

wrapMessage :: JSValue -> String -> JSValue
wrapMessage msg server = obj [ ("command",str "send")
                             , ("server" ,str server)
                             , ("message",msg       )
                             ]
                                                          
userMessage :: String -> String -> JSValue
userMessage username = wrapMessage $ ircMessage Nothing
                                                (str "user")
                                                (JSArray (replicate 4 (str username)))
                                                Nothing
                              
nickMessage :: String -> String -> JSValue
nickMessage username = wrapMessage $ ircMessage Nothing 
                                                (str "nick")
                                                (str username)
                                                Nothing

ircMessage :: Maybe JSValue -> JSValue -> JSValue -> Maybe JSValue -> JSValue
ircMessage (maybeToList -> prefix) cmd params (maybeToList -> text) = obj $
    [ ("prefix"  ,p     ) | p <- prefix ] ++
    [ ("command" ,cmd   )  
    , ("params"  ,params) ] ++
    [ ("text"    ,t     ) | t <- text ]    
    -- how to do this better (not maybeToList + comprehensions?)

