module Server.SVSend
  ( broadcastCommand
  , broadcastPrintf
  , clientPrintf
  , multicast
  , sendClientMessages
  , startSound
  ) where

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import qualified QCommon.MSG as MSG
import           QCommon.NetChanT
import           QCommon.SizeBufT
import qualified QCommon.SZ as SZ
import           QuakeRef
import           QuakeState
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, ix, (^.))
import           Control.Monad (unless, (>=>))
import           Data.Bits (shiftR, shiftL, (.&.))
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
multicast origin to =
  do area1 <- calcArea
     checkServerRecording
     (reliable, mask) <- getReliableAndMask origin to
     sendDataToRelevantClients area1 reliable mask
  where calcArea
          | to /= Constants.multicastAllR && to /= Constants.multicastAll =
              CM.pointLeafNum origin >>= CM.leafArea
          | otherwise = return 0
        checkServerRecording =
          do demoFile <- use (svGlobals.svServerStatic.ssDemoFile)
             maybe (return ()) (const storeEverything) demoFile
        storeEverything =
          do buf <- use (svGlobals.svServer.sMulticast.sbData)
             len <- use (svGlobals.svServer.sMulticast.sbCurSize)
             SZ.write (svGlobals.svServerStatic.ssDemoMulticast) buf len

getReliableAndMask :: V3 Float -> Int -> Quake (Bool, Maybe B.ByteString)
getReliableAndMask origin to
  | to == Constants.multicastAllR = return (True, Nothing)
  | to == Constants.multicastAll = return (False, Nothing)
  | to == Constants.multicastPhsR =
      do m <- calcPHS origin
         return (True, Just m)
  | to == Constants.multicastPhs =
      do m <- calcPHS origin
         return (False, Just m)
  | to == Constants.multicastPvsR =
      do m <- calcPVS origin
         return (True, Just m)
  | to == Constants.multicastPvs =
      do m <- calcPVS origin
         return (False, Just m)
  | otherwise =
      do Com.comError Constants.errFatal ("SV_Multicast: bad to:" `B.append` encode to `B.append` "\n")
         return (False, Nothing)
  where calcPHS = CM.pointLeafNum >=> CM.leafCluster >=> CM.clusterPHS
        calcPVS = CM.pointLeafNum >=> CM.leafCluster >=> CM.clusterPVS

sendDataToRelevantClients :: Int -> Bool -> Maybe B.ByteString -> Quake ()
sendDataToRelevantClients area1 reliable mask =
  do maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
     mapM_ (readClient >=> sendDataToClient area1 reliable mask) [0..maxClients-1]
     SZ.clear (svGlobals.svServer.sMulticast)
  where readClient idx =
          do client <- readRef (Ref idx)
             return (Ref idx, client)

sendDataToClient :: Int -> Bool -> Maybe B.ByteString -> (Ref ClientT, ClientT) -> Quake ()
sendDataToClient area1 reliable mask (Ref idx, client) =
  do done <- checkClient
     unless done $
       do buf <- use (svGlobals.svServer.sMulticast.sbData)
          len <- use (svGlobals.svServer.sMulticast.sbCurSize)
          sendData buf len
  where shouldSkip = (client^.cState) == Constants.csFree || (client^.cState) == Constants.csZombie || ((client^.cState) /= Constants.csSpawned && not reliable)
        checkClient
          | shouldSkip = return True
          | otherwise = maybe (return False) (checkMask (client^.cEdict) area1) mask
        sendData buf len
          | reliable = SZ.write (svGlobals.svServerStatic.ssClients.ix idx.cNetChan.ncMessage) buf len
          | otherwise = SZ.write (svGlobals.svServerStatic.ssClients.ix idx.cDatagram) buf len

checkMask :: Maybe (Ref EdictT) -> Int -> B.ByteString -> Quake Bool
checkMask Nothing _ _ = error "SVSend.sendDataToClient client^.cEdict is Nothing"
checkMask (Just edictRef) area1 mask =
  do edict <- readRef edictRef
     leafNum <- CM.pointLeafNum (edict^.eEntityState.esOrigin)
     cluster <- CM.leafCluster leafNum
     area2 <- CM.leafArea leafNum
     connected <- CM.areasConnected area1 area2
     return (not connected || cluster == -1 || B.index mask (cluster `shiftR` 3) .&. (1 `shiftL` (cluster .&. 7)) == 0)

clientPrintf :: ClientT -> Int -> B.ByteString -> Quake ()
clientPrintf = error "SVSend.clientPrintf" -- TODO

sendClientMessages :: Quake ()
sendClientMessages =
  do state <- use (svGlobals.svServer.sState)
     demoFile <- use (svGlobals.svServer.sDemoFile)
     msgLen <- readDemoMessage state demoFile
     maybe (return ()) sendMessages msgLen
  where readDemoMessage _ Nothing = return (Just 0)
        readDemoMessage state (Just demoFile)
          | state == Constants.ssDemo = proceedReadDemoMessage demoFile =<< pausedCVar
          | otherwise = return (Just 0)
        proceedReadDemoMessage demoFile paused
          | (paused^.cvValue) /= 0 = return (Just 0)
          | otherwise =
              do readBytes <- request (io (BL.hGet h 4))
                 checkDemoCompleted demoFile readBytes
        checkDemoCompleted demoFile readBytes
          | BL.length readBytes /= 4 || runGet getInt readBytes == -1 =
              do demoCompleted
                 return Nothing
          | otherwise = do
              ???
        sendMessages msgLen = error "" -- TODO
