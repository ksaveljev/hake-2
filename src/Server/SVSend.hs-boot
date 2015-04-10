module Server.SVSend where

import Linear.V3 (V3)
import qualified Data.ByteString as B

import Quake
import QuakeState

clientPrintf :: ClientT -> Int -> B.ByteString -> Quake ()
broadcastPrintf :: Int -> B.ByteString -> Quake ()
broadcastCommand :: B.ByteString -> Quake ()
multicast :: V3 Float -> Int -> Quake ()
startSound :: V3 Float -> EdictReference -> Int -> Int -> Float -> Float -> Float -> Quake ()
demoCompleted :: Quake ()
sendClientMessages :: Quake ()
