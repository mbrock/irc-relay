module Main where

import Network
import Control.Monad 

import System.IO 
import System.Environment

main = withSocketsDo $ do
    [addr,port] <- getArgs
    h <- connectTo addr (PortNumber (fromIntegral (read port)))
    hSetBuffering h LineBuffering
    out <- hGetContents h
    hPutStrLn h "hej"
    putStr out
--    forever $ do 
--      putStrLn =<< hGetLine h 
