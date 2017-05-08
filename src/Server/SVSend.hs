{-# LANGUAGE ScopedTypeVariables #-}
module Server.SVSend
    ( broadcastCommand
    , broadcastPrintf
    , clientPrintf
    , multicast
    , sendClientMessages
    , startSound
    ) where

import           Control.Exception     (IOException, handle)
import           Control.Lens          (use, ix, (.=), (^.), (&), (.~))
import           Control.Monad         (when, unless, void, (>=>))
import           Data.Binary.Get       (runGet)
import           Data.Bits             (shiftR, shiftL, (.&.), (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import           Linear                (V3(..))
import           System.IO             (hClose)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified QCommon.CM            as CM
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import qualified QCommon.MSG           as MSG
import           QCommon.NetAdrT
import           QCommon.NetChanT
import qualified QCommon.NetChannel    as NetChannel
import           QCommon.SizeBufT
import qualified QCommon.SZ            as SZ
import           QuakeRef
import           QuakeState
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVEnts         as SVEnts
import qualified Server.SVUser         as SVUser
import qualified Sys.Timer             as Timer
import           Types
import           Util.Binary           (encode, getInt)

import {-# SOURCE #-} qualified Server.SVMain as SVMain

broadcastCommand :: B.ByteString -> Quake ()
broadcastCommand cmd = broadcast =<< use (svGlobals.svServer.sState)
  where
    broadcast state
        | state == 0 = return ()
        | otherwise = do
            MSG.writeByteI (svGlobals.svServer.sMulticast) (fromIntegral Constants.svcStuffText)
            MSG.writeString (svGlobals.svServer.sMulticast) cmd
            multicast (V3 0 0 0) Constants.multicastAllR -- TODO: we send V3 0 0 0 but there is NULL in jake2

broadcastPrintf :: Int -> B.ByteString -> Quake ()
broadcastPrintf = error "SVSend.broadcastPrintf" -- TODO

startSound :: Maybe (V3 Float) -> Ref EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()
startSound maybeOrigin edictRef channel soundIdx volume attenuation timeOfs = do
    when (volume < 0 || volume > 1) $
        Com.fatalError ("SV_StartSound: volume = " `B.append` encode volume)
    when (attenuation < 0 || attenuation > 4) $
        Com.fatalError ("SV_StartSound: attenuation = " `B.append` encode attenuation)
    when (timeOfs < 0 || timeOfs > 0.255) $
        Com.fatalError ("SV_StartSound: timeofs = " `B.append` encode timeOfs)
    edict <- readRef edictRef
    let (usePHS, updatedChannel) | channel .&. 8 /= 0 = (False, channel .&. 7)
                                 | otherwise = (True, channel)
        sendChan = ((edict^.eIndex) `shiftL` 3) .|. (updatedChannel .&. 7)
        flags = composeFlags edict
        origin = maybe (getEdictOrigin edict) id maybeOrigin
    MSG.writeByteI (svGlobals.svServer.sMulticast) (fromIntegral Constants.svcSound)
    MSG.writeByteI (svGlobals.svServer.sMulticast) (fromIntegral flags)
    MSG.writeByteI (svGlobals.svServer.sMulticast) (fromIntegral soundIdx)
    when (flags .&. Constants.sndVolume /= 0) $
        MSG.writeByteF (svGlobals.svServer.sMulticast) (volume * 255)
    when (flags .&. Constants.sndAttenuation /= 0) $
        MSG.writeByteF (svGlobals.svServer.sMulticast) (attenuation * 64)
    when (flags .&. Constants.sndOffset /= 0) $
        MSG.writeByteF (svGlobals.svServer.sMulticast) (timeOfs * 1000)
    when (flags .&. Constants.sndEnt /= 0) $
        MSG.writeShort (svGlobals.svServer.sMulticast) (fromIntegral sendChan)
    when (flags .&. Constants.sndPos /= 0) $
        MSG.writePos (svGlobals.svServer.sMulticast) origin
    let usePHS' = if attenuation == Constants.attnNone then False else usePHS
    if updatedChannel .&. Constants.chanReliable /= 0
      then multicast origin (if usePHS' then Constants.multicastPhsR else Constants.multicastAllR)
      else multicast origin (if usePHS' then Constants.multicastPhs else Constants.multicastAll)
  where
    composeFlags edict =
      let a = if volume /= Constants.defaultSoundPacketVolume then Constants.sndVolume else 0
          b = if attenuation /= Constants.defaultSoundPacketAttenuation then Constants.sndAttenuation else 0
          c = if (edict^.eSvFlags) .&. Constants.svfNoClient /= 0 || (edict^.eSolid) == Constants.solidBsp then Constants.sndPos else 0
          d = Constants.sndEnt
          e = if timeOfs /= 0 then Constants.sndOffset else 0
      in (a .|. b .|. c .|. d .|. e)
    getEdictOrigin edict
        | (edict^.eSolid) == Constants.solidBsp = (edict^.eEntityState.esOrigin) + fmap (* 0.5) ((edict^.eMins) + (edict^.eMaxs))
        | otherwise = edict^.eEntityState.esOrigin

multicast :: V3 Float -> Int -> Quake ()
multicast origin to = do
    area1 <- calcArea
    checkServerRecording
    (reliable, mask) <- getReliableAndMask origin to
    sendDataToRelevantClients area1 reliable mask
  where
    calcArea
        | to /= Constants.multicastAllR && to /= Constants.multicastAll =
            CM.pointLeafNum origin >>= CM.leafArea
        | otherwise = return 0
    checkServerRecording = do
        demoFile <- use (svGlobals.svServerStatic.ssDemoFile)
        maybe (return ()) (const storeEverything) demoFile
    storeEverything = do
        buf <- use (svGlobals.svServer.sMulticast.sbData)
        len <- use (svGlobals.svServer.sMulticast.sbCurSize)
        SZ.write (svGlobals.svServerStatic.ssDemoMulticast) buf len

getReliableAndMask :: V3 Float -> Int -> Quake (Bool, Maybe B.ByteString)
getReliableAndMask origin to
    | to == Constants.multicastAllR = return (True, Nothing)
    | to == Constants.multicastAll = return (False, Nothing)
    | to == Constants.multicastPhsR = do
        m <- calcPHS origin
        return (True, Just m)
    | to == Constants.multicastPhs = do
        m <- calcPHS origin
        return (False, Just m)
    | to == Constants.multicastPvsR = do
        m <- calcPVS origin
        return (True, Just m)
    | to == Constants.multicastPvs =  do
        m <- calcPVS origin
        return (False, Just m)
    | otherwise = do
        Com.comError Constants.errFatal ("SV_Multicast: bad to:" `B.append` encode to `B.append` "\n")
        return (False, Nothing)
  where
    calcPHS = CM.pointLeafNum >=> CM.leafCluster >=> CM.clusterPHS
    calcPVS = CM.pointLeafNum >=> CM.leafCluster >=> CM.clusterPVS

sendDataToRelevantClients :: Int -> Bool -> Maybe B.ByteString -> Quake ()
sendDataToRelevantClients area1 reliable mask = do
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    mapM_ (readClient >=> sendDataToClient area1 reliable mask) [0..maxClients-1]
    SZ.clear (svGlobals.svServer.sMulticast)
  where
    readClient idx = do
        client <- readRef (Ref idx) :: Quake ClientT
        return (Ref idx, client)

sendDataToClient :: Int -> Bool -> Maybe B.ByteString -> (Ref ClientT, ClientT) -> Quake ()
sendDataToClient area1 reliable mask (Ref idx, client) = do
    done <- checkClient
    unless done $ do
        buf <- use (svGlobals.svServer.sMulticast.sbData)
        len <- use (svGlobals.svServer.sMulticast.sbCurSize)
        sendData buf len
  where
    shouldSkip = (client^.cState) == Constants.csFree || (client^.cState) == Constants.csZombie || ((client^.cState) /= Constants.csSpawned && not reliable)
    checkClient
        | shouldSkip = return True
        | otherwise = maybe (return False) (checkMask (client^.cEdict) area1) mask
    sendData buf len
        | reliable = SZ.write (svGlobals.svServerStatic.ssClients.ix idx.cNetChan.ncMessage) buf len
        | otherwise = SZ.write (svGlobals.svServerStatic.ssClients.ix idx.cDatagram) buf len

checkMask :: Maybe (Ref EdictT) -> Int -> B.ByteString -> Quake Bool
checkMask Nothing _ _ = error "SVSend.sendDataToClient client^.cEdict is Nothing"
checkMask (Just edictRef) area1 mask = do
    edict <- readRef edictRef
    leafNum <- CM.pointLeafNum (edict^.eEntityState.esOrigin)
    cluster <- CM.leafCluster leafNum
    area2 <- CM.leafArea leafNum
    connected <- CM.areasConnected area1 area2
    return (not connected || cluster == -1 || B.index mask (cluster `shiftR` 3) .&. (1 `shiftL` (cluster .&. 7)) == 0)

clientPrintf :: ClientT -> Int -> B.ByteString -> Quake ()
clientPrintf = error "SVSend.clientPrintf" -- TODO

sendClientMessages :: Quake ()
sendClientMessages = do
    state <- use (svGlobals.svServer.sState)
    demoFile <- use (svGlobals.svServer.sDemoFile)
    msgLen <- readDemoMessage state demoFile
    maybe (return ()) sendMessages msgLen
  where
    readDemoMessage _ Nothing = return (Just 0)
    readDemoMessage state (Just demoFile)
        | state == Constants.ssDemo = proceedReadDemoMessage demoFile =<< pausedCVar
        | otherwise = return (Just 0)
    proceedReadDemoMessage demoFile paused
        | (paused^.cvValue) /= 0 = return (Just 0)
        | otherwise = do
            readBytes <- io (BL.hGet demoFile 4)
            checkDemoCompleted demoFile readBytes
    checkDemoCompleted demoFile readBytes
        | BL.length readBytes /= 4 || runGet getInt readBytes == -1 = do
            demoCompleted
            return Nothing
        | otherwise = do
            let len = runGet getInt readBytes
            when (len > Constants.maxMsgLen) $
              Com.comError Constants.errDrop "SV_SendClientMessages: msglen > MAX_MSGLEN"
            buf <- io (B.hGet demoFile len)
            svGlobals.svMsgBuf .= buf
            verifyDemoData (B.length buf) len
    verifyDemoData bufLen len
        | bufLen /= len = do
            Com.printf "IOError: reading demo file"
            demoCompleted
            return Nothing
        | otherwise = return (Just len)
    sendMessages _ = do
        maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
        mapM_ (readClient >=> sendClientMessage) [0..maxClients-1]
    readClient idx = do
        client <- readRef (Ref idx) :: Quake ClientT
        return (Ref idx, client)

sendClientMessage :: (Ref ClientT, ClientT) -> Quake ()
sendClientMessage (clientRef, client)
    | (client^.cState) /= 0 = do
        checkReliableMessageOverflow clientRef client
        sendToClient clientRef client =<< use (svGlobals.svServer.sState)
    | otherwise = return ()

checkReliableMessageOverflow :: Ref ClientT -> ClientT -> Quake ()
checkReliableMessageOverflow clientRef@(Ref idx) client
    | client^.cNetChan.ncMessage.sbOverflowed = do -- reliable message overflowed, drop the client
        SZ.clear (svGlobals.svServerStatic.ssClients.ix idx.cNetChan.ncMessage)
        SZ.clear (svGlobals.svServerStatic.ssClients.ix idx.cDatagram)
        broadcastPrintf Constants.printHigh ((client^.cName) `B.append` " overflowed\n")
        SVMain.dropClient clientRef
    | otherwise = return ()

sendToClient :: Ref ClientT -> ClientT -> Int -> Quake ()
sendToClient clientRef@(Ref idx) client state
    | state `elem` [Constants.ssCinematic, Constants.ssDemo, Constants.ssPic] = do
        msg <- use (svGlobals.svMsgBuf)
        NetChannel.transmit (svGlobals.svServerStatic.ssClients.ix idx.cNetChan) (B.length msg) msg
    | (client^.cState) == Constants.csSpawned = do
        flooded <- rateDrop clientRef
        unless flooded $ void (sendClientDatagram clientRef) -- don't overrun bandwidth
    | otherwise = do
        curTime <- Timer.getCurTime
        when ((client^.cNetChan.ncMessage.sbCurSize) /= 0 || curTime - (client^.cNetChan.ncLastSent) > 1000) $
            NetChannel.transmit (svGlobals.svServerStatic.ssClients.ix idx.cNetChan) 0 B.empty

demoCompleted :: Quake ()
demoCompleted = do
    demoFile <- use (svGlobals.svServer.sDemoFile)
    maybe (return ()) closeDemoFile demoFile
    SVUser.nextServer
  where
    closeDemoFile h = do
        ok <- io (handle (\(_ :: IOException) -> return False) (hClose h >> return True))
        unless ok (Com.printf "IOError closing demo file") -- IMPROVE: show exception as well
        svGlobals.svServer.sDemoFile .= Nothing

rateDrop :: Ref ClientT -> Quake Bool
rateDrop clientRef = doRateDrop =<< readRef clientRef
  where
    doRateDrop client
        | (client^.cNetChan.ncRemoteAddress.naType) == Constants.naLoopback = return False -- never drop the loopback
        | otherwise = error "SVSend.rateDrop" -- TODO

sendClientDatagram :: Ref ClientT -> Quake Bool
sendClientDatagram clientRef@(Ref clientIdx) = do
    SVEnts.buildClientFrame clientRef
    SZ.initialize (svGlobals.svMsg) B.empty Constants.maxMsgLen
    svGlobals.svMsg.sbAllowOverflow .= True
    -- send over all the relevant entity_state_t
    -- and the player_state_t
    SVEnts.writeFrameToClient clientRef (svGlobals.svMsg)
    -- copy the accumulated multicast datagram
    -- for this client out to the message
    -- it is necessary for this to be after the WriteEntities
    -- so that entity references will be current
    client <- readRef clientRef
    if client^.cDatagram.sbOverflowed
        then Com.printf (B.concat ["WARNING: datagram overflowed for ", client^.cName, "\n"])
        else SZ.write (svGlobals.svMsg) (client^.cDatagram.sbData) (client^.cDatagram.sbCurSize)
    SZ.clear (svGlobals.svServerStatic.ssClients.ix clientIdx.cDatagram)
    checkOverflow client =<< use (svGlobals.svMsg)
    -- send the datagram
    msg <- use (svGlobals.svMsg)
    NetChannel.transmit (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan) (msg^.sbCurSize) (msg^.sbData)
    -- record the size for rate estimation
    frameNum <- use (svGlobals.svServer.sFrameNum)
    modifyRef clientRef (\v -> v & cMessageSize.ix (frameNum `mod` Constants.rateMessages) .~ (msg^.sbCurSize))
    return True
  where
    checkOverflow client msg =
        when (msg^.sbOverflowed) $ do -- must have room left for the packet header
            Com.printf (B.concat ["WARNING: msg overflowed for ", client^.cName, "\n"])
            SZ.clear (svGlobals.svMsg)

