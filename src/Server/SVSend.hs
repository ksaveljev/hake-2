module Server.SVSend
  ( broadcastCommand
  , broadcastPrintf
  , clientPrintf
  , multicast
  , sendClientMessages
  , startSound
  ) where

import qualified Constants
import qualified QCommon.MSG as MSG
import           QuakeState
import           Server.ServerT
import           Types

import           Control.Lens (use)
import qualified Data.ByteString as B
import           Linear (V3(..))

broadcastCommand :: B.ByteString -> Quake ()
broadcastCommand cmd = broadcast =<< use (svGlobals.svServer.sState)
  where broadcast state
          | state == 0 = return ()
          | otherwise =
              do MSG.writeByteI (svGlobals.svServer.sMulticast) (fromIntegral Constants.svcStuffText)
                 MSG.writeString (svGlobals.svServer.sMulticast) cmd
                 multicast (V3 0 0 0) Constants.multicastAllR -- TODO: we send V3 0 0 0 but there is NULL in jake2

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
