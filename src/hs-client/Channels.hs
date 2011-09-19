module Channels where

import Control.Concurrent.Chan
import Control.Monad

type Message = String -- to be updated

data Channels = Channels { toUI   :: Chan Message
                         , fromUI :: Chan Message
                         }
                
newChannels :: IO Channels                
newChannels = liftM2 Channels newChan newChan
