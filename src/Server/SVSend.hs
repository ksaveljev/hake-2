module Server.SVSend
  ( broadcastCommand
  , broadcastPrintf
  , clientPrintf
  , multicast
  , sendClientMessages
  , startSound
  ) where

import           Types

import qualified Data.ByteString as B
import           Linear (V3)

broadcastCommand :: B.ByteString -> Quake ()
broadcastCommand = error "SVSend.broadcastCommand" -- TODO

broadcastPrintf :: Int -> B.ByteString -> Quake ()
broadcastPrintf = error "SVSend.broadcastPrintf" -- TODO

startSound :: Maybe (V3 Float) -> Ref EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound = error "SVSend.startSound" -- TODO

multicast :: V3 Float -> Int -> Quake ()
multicast = error "SVSend.multicast" -- TODO

clientPrintf :: ClientT -> Int -> B.ByteString -> Quake ()
clientPrintf = error "SVSend.clientPrintf" -- TODO

sendClientMessages :: Quake ()
sendClientMessages = error "SVSend.sendClientMessages" -- TODO