module Server.SVSend
  ( broadcastPrintf
  , clientPrintf
  , multicast
  , startSound
  ) where

import           Types

import qualified Data.ByteString as B
import           Linear (V3)

broadcastPrintf :: Int -> B.ByteString -> Quake ()
broadcastPrintf = error "SVSend.broadcastPrintf" -- TODO

startSound :: Maybe (V3 Float) -> EdictRef -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound = error "SVSend.startSound" -- TODO

multicast :: V3 Float -> Int -> Quake ()
multicast = error "SVSend.multicast" -- TODO

clientPrintf :: ClientT -> Int -> B.ByteString -> Quake ()
clientPrintf = error "SVSend.clientPrintf" -- TODO