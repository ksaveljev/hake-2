{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Server.SVSend where

import Control.Exception (IOException, handle)
import Control.Lens (use, preuse, (.=), (^.), ix, Traversal')
import Control.Monad (when, unless, liftM, void)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Maybe (isJust)
import Data.Traversable (traverse)
import Linear.V3 (V3(..))
import System.IO (hClose)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Quake
import QuakeState
import CVarVariables
import Util.Binary
import qualified Constants
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified QCommon.MSG as MSG
import qualified QCommon.NetChannel as NetChannel
import qualified QCommon.SZ as SZ
import qualified Server.SVMain as SVMain
import qualified Server.SVUser as SVUser

{-
=============================================================================

EVENT MESSAGES

=============================================================================
-}

{-
=================
SV_ClientPrintf

Sends text across to be displayed if the level passes
=================
-}
clientPrintf :: ClientT -> Int -> B.ByteString -> Quake ()
clientPrintf _ _ _ = io (putStrLn "SVSend.clientPrintf") >> undefined -- TODO

{-
=================
SV_BroadcastPrintf

Sends text to all active clients
=================
-}
broadcastPrintf :: Int -> B.ByteString -> Quake ()
broadcastPrintf _ _ = io (putStrLn "SVSend.broadcastPrintf") >> undefined -- TODO

{-
=================
SV_BroadcastCommand

Sends text to all active clients
=================
-}
broadcastCommand :: B.ByteString -> Quake ()
broadcastCommand s = do
    state <- use $ svGlobals.svServer.sState
    when (state /= 0) $ do
      MSG.writeByteI (svGlobals.svServer.sMulticast) Constants.svcStuffText
      MSG.writeString (svGlobals.svServer.sMulticast) s
      multicast (V3 0 0 0) Constants.multicastAllR -- TODO: we send V3 0 0 0 but there is NULL in jake2

{-
=================
SV_Multicast

Sends the contents of sv.multicast to a subset of the clients,
then clears sv.multicast.

MULTICAST_ALL	same as broadcast (origin can be null)
MULTICAST_PVS	send to clients potentially visible from org
MULTICAST_PHS	send to clients potentially hearable from org
=================
-}
multicast :: V3 Float -> Int -> Quake ()
multicast origin to = do
    area1 <- if to /= Constants.multicastAllR && to /= Constants.multicastAll
               then do
                 ln <- CM.pointLeafNum origin
                 a1 <- CM.leafArea ln
                 return a1
               else return 0

    -- if doing a serverrecord, store everything
    demoFile <- use $ svGlobals.svServerStatic.ssDemoFile
    when (isJust demoFile) $ do
      buf <- use $ svGlobals.svServer.sMulticast.sbData
      len <- use $ svGlobals.svServer.sMulticast.sbCurSize
      SZ.write (svGlobals.svServerStatic.ssDemoMulticast) buf len

    (reliable, mask) <- if | to == Constants.multicastAllR -> return (True, Nothing)
                           | to == Constants.multicastAll -> return (False, Nothing)
                           | to == Constants.multicastPhsR -> do
                               ln <- CM.pointLeafNum origin
                               c <- CM.leafCluster ln
                               m <- CM.clusterPHS c
                               return (True, Just m)
                           | to == Constants.multicastPhs -> do
                               ln <- CM.pointLeafNum origin
                               c <- CM.leafCluster ln
                               m <- CM.clusterPHS c
                               return (False, Just m)
                           | to == Constants.multicastPvsR -> do
                               ln <- CM.pointLeafNum origin
                               c <- CM.leafCluster ln
                               m <- CM.clusterPVS c
                               return (True, Just m)
                           | to == Constants.multicastPvs -> do
                               ln <- CM.pointLeafNum origin
                               c <- CM.leafCluster ln
                               m <- CM.clusterPVS c
                               return (False, Just m)
                           | otherwise -> do
                               Com.comError Constants.errFatal ("SV_Multicast: bad to:" `B.append` (BC.pack $ show to) `B.append` "\n") -- IMPROVE
                               return (False, Nothing)

    -- send the data to all relevant clients
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
    sendDataToRelevantClients reliable area1 mask 0 maxClientsValue

    SZ.clear (svGlobals.svServer.sMulticast)

  where sendDataToRelevantClients :: Bool -> Int -> Maybe B.ByteString -> Int -> Int -> Quake ()
        sendDataToRelevantClients reliable area1 mask idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix idx

              let shouldSkip = (client^.cState) == Constants.csFree || (client^.cState) == Constants.csZombie || ((client^.cState) /= Constants.csSpawned && not reliable)

              done <- if shouldSkip
                        then return True
                        else
                          if isJust mask
                            then do
                              let Just (EdictReference edictIdx) = client^.cEdict
                              Just orig <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin
                              leafNum <- CM.pointLeafNum orig
                              cluster <- CM.leafCluster leafNum
                              area2 <- CM.leafArea leafNum
                              connected <- CM.areasConnected area1 area2

                              let maskStuff = if isJust mask
                                                then let Just m = mask
                                                     in (B.index m (cluster `shiftR` 3)) .&. (1 `shiftL` (cluster .&. 7)) == 0
                                                else False

                              if not connected || cluster == -1 {- quake2 bugfix -} || maskStuff
                                then return True
                                else return False
                            else return False

              unless done $ do
                buf <- use $ svGlobals.svServer.sMulticast.sbData
                len <- use $ svGlobals.svServer.sMulticast.sbCurSize

                if reliable
                  then SZ.write (svGlobals.svServerStatic.ssClients.ix idx.cNetChan.ncMessage) buf len
                  else SZ.write (svGlobals.svServerStatic.ssClients.ix idx.cDatagram) buf len

              sendDataToRelevantClients reliable area1 mask (idx + 1) maxIdx

{-
==================
SV_StartSound

Each entity can have eight independant sound sources, like voice,
weapon, feet, etc.

If cahnnel & 8, the sound will be sent to everyone, not just
things in the PHS.

FIXME: if entity isn't in PHS, they must be forced to be sent or
have the origin explicitly sent.

Channel 0 is an auto-allocate channel, the others override anything
already running on that entity/channel pair.

An attenuation of 0 will play full volume everywhere in the level.
Larger attenuations will drop off.  (max 4 attenuation)

Timeofs can range from 0.0 to 0.1 to cause sounds to be started
later in the frame than they normally would.

If origin is null, the origin is determined from the entity origin
or the midpoint of the entity box for bmodels.
==================
-}
startSound :: V3 Float -> EdictReference -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound _ _ _ _ _ _ _ = io (putStrLn "SVsend.startSound") >> undefined -- TODO

{-
=======================
SV_SendClientDatagram
=======================
-}
sendClientDatagram :: Traversal' QuakeState ClientT -> Quake ()
sendClientDatagram _ = io (putStrLn "SVSend.sendClientDatagram") >> undefined -- TODO

{-
==================
SV_DemoCompleted
==================
-}
demoCompleted :: Quake ()
demoCompleted = do
    demofile <- use $ svGlobals.svServer.sDemoFile

    when (isJust demofile) $ do
      let Just h = demofile
      ok <- io $ handle (\(_ :: IOException) -> return False) (hClose h >> return True)

      unless ok $
        Com.printf "IOError closing demo file" -- IMPROVE: show exception as well

      svGlobals.svServer.sDemoFile .= Nothing

    SVUser.nextServer

{-
=======================
SV_RateDrop

Returns true if the client is over its current
bandwidth estimation and should not be sent another packet
=======================
-}
rateDrop :: Traversal' QuakeState ClientT -> Quake Bool
rateDrop _ = io (putStrLn "SVSend.rateDrop") >> undefined -- TODO

{-
=======================
SV_SendClientMessages
=======================
-}
sendClientMessages :: Quake ()
sendClientMessages = do
    state <- use $ svGlobals.svServer.sState
    demofile <- use $ svGlobals.svServer.sDemoFile
    
    -- read the next demo message if needed
    msglen <- if state == Constants.ssDemo && isJust demofile
                then do
                  pausedValue <- liftM (^.cvValue) svPausedCVar

                  if pausedValue /= 0
                    then return (Just 0)
                    else do
                      let Just h = demofile
                      readBytes <- io $ BL.hGet h 4

                      if BL.length readBytes /= 4 || runGet getInt readBytes == -1
                        then do
                          demoCompleted
                          return Nothing
                        else do
                          let len = runGet getInt readBytes

                          when (len > Constants.maxMsgLen) $
                            Com.comError Constants.errDrop "SV_SendClientMessages: msglen > MAX_MSGLEN"

                          r <- io $ B.hGet h len
                          svGlobals.svMsgBuf .= r

                          if B.length r /= len
                            then do
                              Com.printf "IOError: reading demo file"
                              demoCompleted
                              return Nothing
                            else return (Just len)

                else return (Just 0)

    when (isJust msglen) $ do
      -- send a message to each connected client
      maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
      void $ traverse (\idx -> sendMessage (svGlobals.svServerStatic.ssClients.ix idx)) [0..maxClientsValue-1]

  where sendMessage :: Traversal' QuakeState ClientT -> Quake ()
        sendMessage clientLens = do
          Just client <- preuse clientLens

          when ((client^.cState) /= 0) $ do
            -- if the reliable message overflowed, drop the client
            when (client^.cNetChan.ncMessage.sbOverflowed) $ do
              SZ.clear (clientLens.cNetChan.ncMessage)
              SZ.clear (clientLens.cDatagram)
              broadcastPrintf Constants.printHigh ((client^.cName) `B.append` " overflowed\n")
              SVMain.dropClient clientLens

            state <- use $ svGlobals.svServer.sState

            if | elem state [Constants.ssCinematic, Constants.ssDemo, Constants.ssPic] -> do
                   msg <- use $ svGlobals.svMsgBuf
                   NetChannel.transmit (clientLens.cNetChan) (B.length msg) msg
               | state == Constants.csSpawned -> do
                   flooded <- rateDrop clientLens
                   -- don't overrun bandwidth
                   unless flooded $ sendClientDatagram clientLens
               | otherwise -> do
                   Just netChan <- preuse $ clientLens.cNetChan
                   curTime <- use $ globals.curtime
                   -- just update reliable if needed
                   when ((netChan^.ncMessage.sbCurSize) /= 0 || curTime - (netChan^.ncLastSent) > 1000) $
                     NetChannel.transmit (clientLens.cNetChan) 0 ""
