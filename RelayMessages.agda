
-- This Agda file serves as documentation for the messages client to/from relay.
-- Use type families/associated types to do this in Haskell

-- http://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands

module RelayMessages where

open import Data.String
open import Data.List
open import Data.Nat
open import Data.Maybe

Server : Set
Server = String

data Command : Set where
  CONNECT USER NICK : Command

record ConnectContent : Set where
  constructor connect
  field
    server : String
    port   : ℕ

record UserContent : Set where
  constructor user
  field
    username   : String
    hostname   : String
    servername : String
    realname   : String

record NickContent : Set where
  constructor nick
  field
    nickname : String

Content : Command → Set
Content CONNECT = ConnectContent
Content USER    = UserContent
Content NICK    = NickContent

-- Message ≈ Σ Command Content
record Message : Set where
  constructor message
  -- Lacking a server field
  field
    command : Command
    content : Content command
    
private
  connectMsg : Message
  connectMsg = record 
    { command = CONNECT
    ; content = connect "irc.freenode.net" 6667
    }

record IRCCommand : Set where
  constructor _,_,_,_
  field
    prefix  : Maybe String
    command : Command
    params  : List String
    text    : Maybe String

toIRCCommand : Message → IRCCommand
toIRCCommand (message CONNECT (connect server port))                     = nothing , CONNECT , server ∷ {- show port ∷ -} []                     , nothing
toIRCCommand (message USER (user username hostname servername realname)) = nothing , USER    , username ∷ hostname ∷ servername ∷ realname ∷ [] , nothing
toIRCCommand (message NICK (nick nickname))                              = nothing , NICK    , nickname ∷ []                                     , nothing
  
  