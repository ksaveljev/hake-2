module Server.SVSend where

import Linear.V3 (V3)
import qualified Data.ByteString as B

import Types
import QuakeState

clientPrintf :: ClientT -> Int -> B.ByteString -> Quake ()

broadcastPrintf :: Int -> B.ByteString -> Quake ()

broadcastCommand :: B.ByteString -> Quake ()

multicast :: V3 Float -> Int -> Quake ()

startSound :: Maybe (V3 Float) -> Ref EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()

demoCompleted :: Quake ()

sendClientMessages :: Quake ()
