{-# LANGUAGE Rank2Types #-}
module Server.SVEnts
    ( buildClientFrame
    , recordDemoMessage
    , writeFrameToClient
    ) where

import           Control.Lens             (Lens', Traversal', use, ix, (.=), (+=), (^.), (&), (.~))
import           Control.Monad            (when)
import           Data.Bits                (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI
import           Data.Maybe               (fromJust)
import qualified Data.Vector              as V
import qualified Data.Vector.Storable     as SV
import qualified Data.Vector.Unboxed      as UV
import           Linear                   (V3(..), norm, _x, _y, _z, _w)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import           Game.GClientT
import           Game.PlayerStateT
import           Game.PMoveStateT
import qualified QCommon.CM               as CM
import qualified QCommon.Com              as Com
import           QCommon.CVarVariables
import qualified QCommon.MSG              as MSG
import qualified QCommon.SZ               as SZ
import           QuakeRef
import           QuakeState
import           Server.ClientFrameT
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import           Types

-- TODO: old code, refactoring is needed
buildClientFrame :: Ref ClientT -> Quake ()
buildClientFrame clientRef@(Ref clientIdx) = do
    client <- readRef clientRef
    clEntRef <- getClientEntRef (client^.cEdict)
    clEnt <- readRef clEntRef
    maybe (return ()) (doBuildClientFrame clEntRef) (clEnt^.eClient)
  where
    getClientEntRef Nothing = do
        Com.fatalError "SVEnts.buildClientFrame client^.cEdict is Nothing"
        return (Ref (-1))
    getClientEntRef (Just clEntRef) = return clEntRef
    doBuildClientFrame clEntRef gClientRef = do
        gClient <- readRef gClientRef
        frameNum <- use (svGlobals.svServer.sFrameNum)
        realTime <- use (svGlobals.svServerStatic.ssRealTime) -- save it for ping calc later
        let frameIdx = frameNum .&. Constants.updateMask
            frame = svGlobals.svServerStatic.ssClients.ix clientIdx.cFrames.ix frameIdx :: Traversal' QuakeState ClientFrameT
            org = (gClient^.gcPlayerState.psViewOffset) + fmap ((* 0.125) . fromIntegral) (gClient^.gcPlayerState.psPMoveState.pmsOrigin)
        frame.cfSentTime .= realTime
        leafNum <- CM.pointLeafNum org
        clientArea <- CM.leafArea leafNum
        clientCluster <- CM.leafCluster leafNum
        CM.writeAreaBits (svGlobals.svServerStatic.ssClients.ix clientIdx.cFrames.ix frameIdx.cfAreaBits) clientArea >>= (frame.cfAreaBytes .=)
        frame.cfPlayerState .= (gClient^.gcPlayerState)
        fatPVS org
        clientPHS <- CM.clusterPHS clientCluster
        frame.cfNumEntities .= 0
        use (svGlobals.svServerStatic.ssNextClientEntities) >>= (frame.cfFirstEntity .=)
        numEdicts <- use (gameBaseGlobals.gbNumEdicts)
        collectEdicts org clientPHS clientArea clEntRef frame 1 numEdicts
    collectEdicts org clientPHS clientArea clEntRef frame idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            edict <- readRef (Ref idx)
            case () of
                _ | (edict^.eSvFlags) .&. Constants.svfNoClient /= 0 -> do
                      collectEdicts org clientPHS clientArea clEntRef frame (idx + 1) maxIdx
                  | (edict^.eEntityState.esModelIndex) == 0 && (edict^.eEntityState.esEffects) == 0 && (edict^.eEntityState.esSound) == 0 && (edict^.eEntityState.esEvent) == 0 -> do
                      collectEdicts org clientPHS clientArea clEntRef frame (idx + 1) maxIdx
                  | otherwise -> do
                      skip <- if Ref idx /= clEntRef
                                then do
                                  blocked <- isBlockedByDoor clientArea edict
                                  if blocked
                                    then do
                                      return True
                                    else
                                      if (edict^.eEntityState.esRenderFx) .&. Constants.rfBeam /= 0
                                        then do
                                          let l = (edict^.eClusterNums) UV.! 0
                                          if (clientPHS `B.index` (l `shiftR` 3)) .&. (1 `shiftL` (l .&. 7)) == 0
                                            then do
                                              return True
                                            else return False
                                        else do
                                          -- FIXME: if an ent has a model and a sound, but isn't
                                          -- in the PVS, only the PHS, clear the model
                                          bitVector <- if (edict^.eEntityState.esSound) == 0
                                                         then use $ svGlobals.svFatPVS -- return clientPHS
                                                         else use $ svGlobals.svFatPVS
                                          done <- if (edict^.eNumClusters) == -1 -- too many leafs for individual check, go by headnode
                                                    then do
                                                      visible <- CM.headNodeVisible (edict^.eHeadNode) bitVector
                                                      return (not visible)
                                                    else do -- check individual leafs
                                                      let visible = checkBitVector edict bitVector 0 (edict^.eNumClusters)
                                                      return (not visible)
                                          if done
                                            then do
                                              return True
                                            else
                                              if (edict^.eEntityState.esModelIndex) == 0 -- don't send sounds if they will be attenuated away
                                                then do
                                                  let delta = org - (edict^.eEntityState.esOrigin)
                                                      len = norm delta
                                                  if len > 400
                                                    then do
                                                      return True
                                                    else return False
                                                else
                                                  return False
                                else
                                  return False
                      if skip
                        then do
                          collectEdicts org clientPHS clientArea clEntRef frame (idx + 1) maxIdx
                        else do
                          serverStatic <- use $ svGlobals.svServerStatic
                          let index = (serverStatic^.ssNextClientEntities) `mod` (serverStatic^.ssNumClientEntities)
                              -- state = (serverStatic^.ssClientEntities) V.! index
                          when ((edict^.eEntityState.esNumber) /= idx) $ do
                            Com.dprintf "FIXING ENT.S.NUMBER!!!\n"
                            modifyRef (Ref idx) (\v -> v & eEntityState.esNumber .~ idx)
                          readRef (Ref idx) >>= \edict' ->
                            svGlobals.svServerStatic.ssClientEntities.ix index .= (edict'^.eEntityState)
                          readRef clientRef >>= \client ->
                            when ((edict^.eOwner) == (client^.cEdict)) $
                              svGlobals.svServerStatic.ssClientEntities.ix index.esSolid .= 0
                          svGlobals.svServerStatic.ssNextClientEntities += 1
                          frame.cfNumEntities += 1
                          collectEdicts org clientPHS clientArea clEntRef frame (idx + 1) maxIdx
    
    isBlockedByDoor clientArea edict = do
        connected <- CM.areasConnected clientArea (edict^.eAreaNum)
        checkIfBlockedByDoor clientArea edict connected
    checkIfBlockedByDoor clientArea edict connected
        | not connected && (edict^.eAreaNum2) == 0 = return True
        | not connected = fmap not (CM.areasConnected clientArea (edict^.eAreaNum2))
        | otherwise = return False
    checkBitVector edict bitVector idx maxIdx
        | idx >= maxIdx = False
        | otherwise =
            let l = (edict^.eClusterNums) UV.! idx
            in if (bitVector UV.! (l `shiftR` 3)) .&. (1 `shiftL` (l .&. 7)) /= 0
                   then True
                   else checkBitVector edict bitVector (idx + 1) maxIdx

recordDemoMessage :: Quake ()
recordDemoMessage = do
    demoFile <- use (svGlobals.svServerStatic.ssDemoFile)
    maybe (return ()) doRecordDemoMessage demoFile
  where
    doRecordDemoMessage demoFile = error "SVEnts.recordDemoMessage" -- TODO

writeFrameToClient :: Ref ClientT -> Lens' QuakeState SizeBufT -> Quake ()
writeFrameToClient clientRef@(Ref clientIdx) sizeBufLens = do
    client <- readRef clientRef
    frameNum <- use (svGlobals.svServer.sFrameNum)
    let frameIdx = frameNum .&. Constants.updateMask
        frame = (client^.cFrames) V.! frameIdx
        (oldFrame, lastFrame)
            | (client^.cLastFrame) <= 0 = (Nothing, -1) -- client is asking for a retransmit
            | frameNum - (client^.cLastFrame) >= Constants.updateBackup - 3 = (Nothing, -1) -- client hasn't gotten a good message through in a long time
            | otherwise = (Just ((client^.cFrames) V.! ((client^.cLastFrame) .&. Constants.updateMask)), client^.cLastFrame)
    MSG.writeByteI sizeBufLens Constants.svcFrame
    MSG.writeLong sizeBufLens frameNum
    MSG.writeLong sizeBufLens lastFrame -- what we are delta'ing from
    MSG.writeByteI sizeBufLens (client^.cSurpressCount) -- rate dropped packets
    modifyRef clientRef (\v -> v & cSurpressCount .~ 0)
    -- send over the areabits
    MSG.writeByteI sizeBufLens (frame^.cfAreaBytes)
    let (ptr, n) = SV.unsafeToForeignPtr0 (frame^.cfAreaBits)
        areaBits = BI.PS ptr 0 n
    SZ.write sizeBufLens areaBits (frame^.cfAreaBytes)
    -- delta encode the playerstate
    writePlayerStateToClient oldFrame frame sizeBufLens
    -- delta encode the entities
    emitPacketEntities oldFrame frame sizeBufLens

writePlayerStateToClient :: Maybe ClientFrameT -> ClientFrameT -> Lens' QuakeState SizeBufT -> Quake ()
writePlayerStateToClient from to sizeBufLens = do
    let ps = to^.cfPlayerState
        ops = maybe newPlayerStateT (^.cfPlayerState) from
        a = if (ps^.psPMoveState.pmsPMType) /= (ops^.psPMoveState.pmsPMType)
                then Constants.psMType else 0
        b = if (ps^.psPMoveState.pmsOrigin) /= (ops^.psPMoveState.pmsOrigin)
                then Constants.psMOrigin else 0
        c = if (ps^.psPMoveState.pmsVelocity) /= (ops^.psPMoveState.pmsVelocity)
                then Constants.psMVelocity else 0
        d = if (ps^.psPMoveState.pmsPMTime) /= (ops^.psPMoveState.pmsPMTime)
                then Constants.psMTime else 0
        e = if (ps^.psPMoveState.pmsPMFlags) /= (ops^.psPMoveState.pmsPMFlags)
                then Constants.psMFlags else 0
        f = if (ps^.psPMoveState.pmsGravity) /= (ops^.psPMoveState.pmsGravity)
                then Constants.psMGravity else 0
        g = if (ps^.psPMoveState.pmsDeltaAngles) /= (ops^.psPMoveState.pmsDeltaAngles)
                then Constants.psMDeltaAngles else 0
        h = if (ps^.psViewAngles) /= (ops^.psViewAngles)
                then Constants.psViewAngles else 0
        i = if (ps^.psViewOffset) /= (ops^.psViewOffset)
                then Constants.psViewOffset else 0
        j = if (ps^.psKickAngles) /= (ops^.psKickAngles)
                then Constants.psKickAngles else 0
        k = if (ps^.psBlend) /= (ops^.psBlend)
                then Constants.psBlend else 0
        l = if (ps^.psFOV) /= (ops^.psFOV)
                then Constants.psFov else 0
        m = if (ps^.psRDFlags) /= (ops^.psRDFlags)
                then Constants.psRdFlags else 0
        n = if (ps^.psGunFrame) /= (ops^.psGunFrame)
                then Constants.psWeaponFrame else 0
        o = Constants.psWeaponIndex
        pflags = a .|. b .|. c .|. d .|. e .|. f .|. g .|. h .|. i .|. j .|. k .|. l .|. m .|. n .|. o
    MSG.writeByteI sizeBufLens Constants.svcPlayerInfo
    MSG.writeShort sizeBufLens pflags
    -- write the pmove_state_t
    when (pflags .&. Constants.psMType /= 0) $
        MSG.writeByteI sizeBufLens (ps^.psPMoveState.pmsPMType)
    when (pflags .&. Constants.psMOrigin /= 0) $ do
        MSG.writeShort sizeBufLens (fromIntegral (ps^.psPMoveState.pmsOrigin._x))
        MSG.writeShort sizeBufLens (fromIntegral (ps^.psPMoveState.pmsOrigin._y))
        MSG.writeShort sizeBufLens (fromIntegral (ps^.psPMoveState.pmsOrigin._z))
    when (pflags .&. Constants.psMVelocity /= 0) $ do
        MSG.writeShort sizeBufLens (fromIntegral (ps^.psPMoveState.pmsVelocity._x))
        MSG.writeShort sizeBufLens (fromIntegral (ps^.psPMoveState.pmsVelocity._y))
        MSG.writeShort sizeBufLens (fromIntegral (ps^.psPMoveState.pmsVelocity._z))
    when (pflags .&. Constants.psMTime /= 0) $
        MSG.writeByteI sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsPMTime)
    when (pflags .&. Constants.psMFlags /= 0) $
        MSG.writeByteI sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsPMFlags)
    when (pflags .&. Constants.psMGravity /= 0) $
        MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsGravity)
    when (pflags .&. Constants.psMDeltaAngles /= 0) $ do
        MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsDeltaAngles._x)
        MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsDeltaAngles._y)
        MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsDeltaAngles._z)
    when (pflags .&. Constants.psViewOffset /= 0) $ do
        MSG.writeCharF sizeBufLens ((ps^.psViewOffset._x) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psViewOffset._y) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psViewOffset._z) * 4)
    when (pflags .&. Constants.psViewAngles /= 0) $ do
        MSG.writeAngle16 sizeBufLens (ps^.psViewAngles._x)
        MSG.writeAngle16 sizeBufLens (ps^.psViewAngles._y)
        MSG.writeAngle16 sizeBufLens (ps^.psViewAngles._z)
    when (pflags .&. Constants.psKickAngles /= 0) $ do
        MSG.writeCharF sizeBufLens ((ps^.psKickAngles._x) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psKickAngles._y) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psKickAngles._z) * 4)
    when (pflags .&. Constants.psWeaponIndex /= 0) $
        MSG.writeByteI sizeBufLens (ps^.psGunIndex)
    when (pflags .&. Constants.psWeaponFrame /= 0) $ do
        MSG.writeByteI sizeBufLens (ps^.psGunFrame)
        MSG.writeCharF sizeBufLens ((ps^.psGunOffset._x) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psGunOffset._y) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psGunOffset._z) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psGunAngles._x) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psGunAngles._y) * 4)
        MSG.writeCharF sizeBufLens ((ps^.psGunAngles._z) * 4)
    when (pflags .&. Constants.psBlend /= 0) $ do
        MSG.writeByteF sizeBufLens ((ps^.psBlend._x) * 255)
        MSG.writeByteF sizeBufLens ((ps^.psBlend._y) * 255)
        MSG.writeByteF sizeBufLens ((ps^.psBlend._z) * 255)
        MSG.writeByteF sizeBufLens ((ps^.psBlend._w) * 255)
    when (pflags .&. Constants.psFov /= 0) $
        MSG.writeByteF sizeBufLens (ps^.psFOV)
    when (pflags .&. Constants.psRdFlags /= 0) $
        MSG.writeByteI sizeBufLens (ps^.psRDFlags)
    -- send stats
    let statbits = calcStatBits ps ops 0 0 Constants.maxStats
    MSG.writeLong sizeBufLens statbits
    writeStats ps statbits 0 Constants.maxStats
  where
    calcStatBits ps ops statbits idx maxIdx
        | idx >= maxIdx = statbits
        | otherwise = if ((ps^.psStats) UV.! idx) /= ((ops^.psStats) UV.! idx)
                          then calcStatBits ps ops (statbits .|. (1 `shiftL` idx)) (idx + 1) maxIdx
                          else calcStatBits ps ops statbits (idx + 1) maxIdx
    writeStats ps statbits idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            when (statbits .&. (1 `shiftL` idx) /= 0) $
                MSG.writeShort sizeBufLens (fromIntegral $ (ps^.psStats) UV.! idx)
            writeStats ps statbits (idx + 1) maxIdx

-- TODO: this begs for refactoring
emitPacketEntities :: Maybe ClientFrameT -> ClientFrameT -> Lens' QuakeState SizeBufT -> Quake ()
emitPacketEntities from to sizeBufLens = do
    let fromNumEntities = maybe 0 (^.cfNumEntities) from
    MSG.writeByteI sizeBufLens Constants.svcPacketEntities
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    numClientEntities <- use (svGlobals.svServerStatic.ssNumClientEntities)
    sendEntities maxClients numClientEntities fromNumEntities Nothing Nothing 0 0
    MSG.writeShort sizeBufLens 0 -- end of packetentities
  where
    sendEntities maxClients numClientEntities fromNumEntites oldEnt newEnt oldIndex newIndex
        | newIndex >= (to^.cfNumEntities) && oldIndex >= fromNumEntites = return ()
        | otherwise = do
            clientEntities <- use (svGlobals.svServerStatic.ssClientEntities)
            (newEnt', newNum) <- if newIndex >= (to^.cfNumEntities)
                                     then return (newEnt, 9999)
                                     else do
                                         let idx = ((to^.cfFirstEntity) + newIndex) `mod` numClientEntities
                                             newEnt' = clientEntities V.! idx
                                         return (Just newEnt', newEnt'^.esNumber)
            (oldEnt', oldNum) <- if oldIndex >= fromNumEntites
                                     then return (oldEnt, 9999)
                                     else do
                                         let idx = ((fromJust from^.cfFirstEntity) + oldIndex) `mod` numClientEntities
                                             oldEnt' = clientEntities V.! idx
                                         return (Just oldEnt', oldEnt'^.esNumber)
            case () of
                _ | newNum == oldNum -> do
                      MSG.writeDeltaEntity (fromJust oldEnt') (fromJust newEnt') sizeBufLens False (((fromJust newEnt')^.esNumber) <= maxClients)
                      sendEntities maxClients numClientEntities fromNumEntites oldEnt' newEnt' (oldIndex + 1) (newIndex + 1)
                  | newNum < oldNum -> do
                      baselines <- use (svGlobals.svServer.sBaselines)
                      let baseline = baselines V.! newNum
                      MSG.writeDeltaEntity baseline (fromJust newEnt') sizeBufLens True True
                      sendEntities maxClients numClientEntities fromNumEntites oldEnt' newEnt' oldIndex (newIndex + 1)
                  | newNum > oldNum -> do
                      let bits = if oldNum >= 256
                                     then Constants.uRemove .|. Constants.uNumber16 .|. Constants.uMoreBits1
                                     else Constants.uRemove
                      MSG.writeByteI sizeBufLens (bits .&. 255)
                      when (bits .&. 0x0000FF00 /= 0) $
                          MSG.writeByteI sizeBufLens ((bits `shiftR` 8) .&. 255)
                      if bits .&. Constants.uNumber16 /= 0
                          then MSG.writeShort sizeBufLens oldNum
                          else MSG.writeByteI sizeBufLens oldNum
                      sendEntities maxClients numClientEntities fromNumEntites oldEnt' newEnt' (oldIndex + 1) newIndex

fatPVS :: V3 Float -> Quake ()
fatPVS org = do
    (count, _) <- CM.boxLeafNums mins maxs (svGlobals.svLeafsTmp) 64 Nothing
    when (count < 1) $
        Com.fatalError "SV_FatPVS: count < 1"
    longs <- fmap (\numClusters -> (numClusters + 31) `shiftR` 5) (use (cmGlobals.cmNumClusters))
    -- convert leafs to clusters
    leafs <- use (svGlobals.svLeafsTmp)
    clusters <- UV.generateM count (\idx -> CM.leafCluster (leafs UV.! idx))
    pvs <- CM.clusterPVS (clusters UV.! 0)
    fatPVS <- use (svGlobals.svFatPVS)
    -- System.arraycopy(CM.CM_ClusterPVS(leafs[0]), 0, SV_ENTS.fatpvs, 0, longs << 2);
    let updatedFatPVS = UV.generate (UV.length fatPVS) (updateFatPVS pvs fatPVS (longs * 4))
    newFatPVS <- calcNewFatPVS updatedFatPVS clusters longs 1 count
    svGlobals.svFatPVS .= newFatPVS
  where
    mins = fmap (subtract 8) org
    maxs = fmap (+ 8) org
    updateFatPVS pvs fatPVS longs idx
        | idx < longs = pvs `B.index` idx
        | otherwise = fatPVS UV.! idx
    calcNewFatPVS fatPVS clusters longs i count -- TODO: most likely needs refactoring (code wise and performance wise)
        | i >= count = return fatPVS
        | otherwise = do
            let j = getJ clusters 0 i
            if j /= i
                then calcNewFatPVS fatPVS clusters longs (i + 1) count -- already have the cluster we want
                else do
                    src <- CM.clusterPVS (clusters UV.! i)
                    let updates = constructFatPVSUpdates fatPVS src 0 0 longs []
                    calcNewFatPVS (fatPVS UV.// updates) clusters longs (i + 1) count
    getJ clusters i j
        | j >= i = j
        | (clusters UV.! i) == (clusters UV.! j) = j
        | otherwise = getJ clusters i (j + 1)
    constructFatPVSUpdates fatPVS src k j longs acc
        | j >= longs = acc
        | otherwise =
            constructFatPVSUpdates fatPVS src (k + 4) (j + 1) longs ((k + 0, (fatPVS UV.! (k + 0)) .|. (src `B.index` (k + 0))) 
                                                                   : (k + 1, (fatPVS UV.! (k + 1)) .|. (src `B.index` (k + 1)))
                                                                   : (k + 2, (fatPVS UV.! (k + 2)) .|. (src `B.index` (k + 2)))
                                                                   : (k + 3, (fatPVS UV.! (k + 3)) .|. (src `B.index` (k + 3))) 
                                                                   : acc)
