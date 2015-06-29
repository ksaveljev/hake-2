{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Server.SVEnts where

import Control.Lens (use, Lens', (^.), preuse, ix, Traversal', (.=), (+=))
import Control.Monad (when, liftM)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Maybe (isJust, fromJust)
import Data.Word (Word8)
import Linear (V3, _x, _y, _z, _w, norm)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified QCommon.MSG as MSG
import qualified QCommon.SZ as SZ

{-
- Save everything in the world out without deltas. Used for recording
- footage for merged or assembled demos.
-}
recordDemoMessage :: Quake ()
recordDemoMessage = do
    demoFile <- use $ svGlobals.svServerStatic.ssDemoFile

    when (isJust demoFile) $ do
      io (putStrLn "SVEnts.recordDemoMessage") >> undefined -- TODO

-- Writes a frame to a client system.
writeFrameToClient :: ClientReference -> Lens' QuakeState SizeBufT -> Quake ()
writeFrameToClient clientRef@(ClientReference clientIdx) sizeBufLens = do
    Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx
    frameNum <- use $ svGlobals.svServer.sFrameNum

    io (print $ "frameNum = " ++ show frameNum)

    let frameIdx = frameNum .&. Constants.updateMask
        frame = svGlobals.svServerStatic.ssClients.ix clientIdx.cFrames.ix frameIdx :: Traversal' QuakeState ClientFrameT

    (oldFrame, lastFrame) <- if | (client^.cLastFrame) <= 0 -> -- client is asking for a retransmit
                                    return (Nothing, -1)
                                | frameNum - (client^.cLastFrame) >= (Constants.updateBackup - 3) ->
                                    -- client hasn't gotten a good message though in a long time
                                    return (Nothing, -1)
                                | otherwise -> do -- we have a valid message to delta from
                                    oldFrame <- preuse (svGlobals.svServerStatic.ssClients.ix clientIdx.cFrames.ix ((client^.cLastFrame) .&. Constants.updateMask))
                                    return (oldFrame, client^.cLastFrame)

    MSG.writeByteI sizeBufLens Constants.svcFrame
    MSG.writeLong sizeBufLens frameNum
    MSG.writeLong sizeBufLens lastFrame -- what we are delta'ing from
    MSG.writeByteI sizeBufLens (client^.cSurpressCount) -- rate dropped packets

    svGlobals.svServerStatic.ssClients.ix clientIdx.cSurpressCount .= 0

    -- send over the areabits
    Just frame' <- preuse frame
    MSG.writeByteI sizeBufLens (frame'^.cfAreaBytes)
    let (ptr, n) = VS.unsafeToForeignPtr0 (frame'^.cfAreaBits)
        areaBits = BI.PS ptr 0 n
    SZ.write sizeBufLens areaBits (frame'^.cfAreaBytes)

    -- delta encode the playerstate
    writePlayerStateToClient oldFrame frame' sizeBufLens
    -- delta encode the entities
    emitPacketEntities oldFrame frame' sizeBufLens

-- Writes the status of a player to a client system.
writePlayerStateToClient :: Maybe ClientFrameT -> ClientFrameT -> Lens' QuakeState SizeBufT -> Quake ()
writePlayerStateToClient from to sizeBufLens = do
    let ps = to^.cfPlayerState
        ops = case from of
                Nothing -> newPlayerStateT
                Just clientFrame -> clientFrame^.cfPlayerState

    -- determine what needs to be sent
    let a = if (ps^.psPMoveState.pmsPMType) /= (ops^.psPMoveState.pmsPMType)
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

    -- write it
    MSG.writeByteI sizeBufLens Constants.svcPlayerInfo
    MSG.writeShort sizeBufLens pflags

    -- write the pmove_state_t
    when (pflags .&. Constants.psMType /= 0) $ do
      io (print "PS_M_TYPE")
      MSG.writeByteI sizeBufLens (ps^.psPMoveState.pmsPMType)

    when (pflags .&. Constants.psMOrigin /= 0) $ do
      io (print "PS_M_ORIGIN")
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsOrigin._x)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsOrigin._y)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsOrigin._z)

    when (pflags .&. Constants.psMVelocity /= 0) $ do
      io (print "PS_M_VELOCITY")
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsVelocity._x)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsVelocity._y)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsVelocity._z)

    when (pflags .&. Constants.psMTime /= 0) $ do
      io (print "PS_M_TIME")
      MSG.writeByteI sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsPMTime)

    when (pflags .&. Constants.psMFlags /= 0) $ do
      io (print "PS_M_FLAGS")
      MSG.writeByteI sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsPMFlags)

    when (pflags .&. Constants.psMGravity /= 0) $ do
      io (print "PS_M_GRAVITY")
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsGravity)

    when (pflags .&. Constants.psMDeltaAngles /= 0) $ do
      io (print "PS_M_DELTA_ANGLES")
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsDeltaAngles._x)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsDeltaAngles._y)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsDeltaAngles._z)

    -- write the rest of the player_state_t
    when (pflags .&. Constants.psViewOffset /= 0) $ do
      io (print "PS_VIEWOFFSET")
      MSG.writeCharF sizeBufLens ((ps^.psViewOffset._x) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psViewOffset._y) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psViewOffset._z) * 4)

    when (pflags .&. Constants.psViewAngles /= 0) $ do
      io (print "PS_VIEWANGLES")
      MSG.writeAngle16 sizeBufLens (ps^.psViewAngles._x)
      MSG.writeAngle16 sizeBufLens (ps^.psViewAngles._y)
      MSG.writeAngle16 sizeBufLens (ps^.psViewAngles._z)

    when (pflags .&. Constants.psKickAngles /= 0) $ do
      io (print "PS_KICKANGLES")
      MSG.writeCharF sizeBufLens ((ps^.psKickAngles._x) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psKickAngles._y) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psKickAngles._z) * 4)

    when (pflags .&. Constants.psWeaponIndex /= 0) $ do
      io (print "PS_WEAPONINDEX")
      MSG.writeByteI sizeBufLens (ps^.psGunIndex)

    when (pflags .&. Constants.psWeaponFrame /= 0) $ do
      io (print "PS_WEAPONFRAME")
      MSG.writeByteI sizeBufLens (ps^.psGunFrame)
      MSG.writeCharF sizeBufLens ((ps^.psGunOffset._x) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psGunOffset._y) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psGunOffset._z) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psGunAngles._x) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psGunAngles._y) * 4)
      MSG.writeCharF sizeBufLens ((ps^.psGunAngles._z) * 4)

    when (pflags .&. Constants.psBlend /= 0) $ do
      io (print "PS_BLEND")
      MSG.writeByteF sizeBufLens ((ps^.psBlend._x) * 255)
      MSG.writeByteF sizeBufLens ((ps^.psBlend._y) * 255)
      MSG.writeByteF sizeBufLens ((ps^.psBlend._z) * 255)
      MSG.writeByteF sizeBufLens ((ps^.psBlend._w) * 255)

    when (pflags .&. Constants.psFov /= 0) $ do
      io (print "PS_FOV")
      MSG.writeByteF sizeBufLens (ps^.psFOV)

    when (pflags .&. Constants.psRdFlags /= 0) $ do
      io (print "PS_RDFLAGS")
      MSG.writeByteI sizeBufLens (ps^.psRDFlags)

    -- send stats
    let statbits = calcStatBits ps ops 0 0 Constants.maxStats

    io (print "statbits")
    MSG.writeLong sizeBufLens statbits

    writeStats ps statbits 0 Constants.maxStats

  where calcStatBits :: PlayerStateT -> PlayerStateT -> Int -> Int -> Int -> Int
        calcStatBits ps ops statbits idx maxIdx
          | idx >= maxIdx = statbits
          | otherwise = if ((ps^.psStats) UV.! idx) /= ((ops^.psStats) UV.! idx)
                          then calcStatBits ps ops (statbits .|. (1 `shiftL` idx)) (idx + 1) maxIdx
                          else calcStatBits ps ops statbits (idx + 1) maxIdx

        writeStats :: PlayerStateT -> Int -> Int -> Int -> Quake ()
        writeStats ps statbits idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              when (statbits .&. (1 `shiftL` idx) /= 0) $
                MSG.writeShort sizeBufLens (fromIntegral $ (ps^.psStats) UV.! idx)

              writeStats ps statbits (idx + 1) maxIdx

-- Writes a delta update of an entity_state_t list to the message
emitPacketEntities :: Maybe ClientFrameT -> ClientFrameT -> Lens' QuakeState SizeBufT -> Quake ()
emitPacketEntities from to sizeBufLens = do
    MSG.writeByteI sizeBufLens Constants.svcPacketEntities

    let fromNumEntites = case from of
                           Nothing -> 0
                           Just clientFrame -> clientFrame^.cfNumEntities

    maxClientsValue <- liftM (^.cvValue) maxClientsCVar
    numClientEntities <- use $ svGlobals.svServerStatic.ssNumClientEntities
    sendEntities (truncate maxClientsValue) numClientEntities fromNumEntites Nothing Nothing 0 0

    MSG.writeShort sizeBufLens 0 -- end of packetentities

  where sendEntities :: Int -> Int -> Int -> Maybe EntityStateT -> Maybe EntityStateT -> Int -> Int -> Quake ()
        sendEntities maxClientsValue numClientEntities fromNumEntites oldEnt newEnt oldIndex newIndex
          | newIndex >= (to^.cfNumEntities) && oldIndex >= fromNumEntites = return ()
          | otherwise = do
              io (print $ "maxClientsValue = " ++ show maxClientsValue)
              io (print $ "numClientEntities = " ++ show numClientEntities)
              io (print $ "fromNumEntites = " ++ show fromNumEntites)
              io (print $ "cfNumEntities = " ++ show (to^.cfNumEntities))
              io (print $ "newIndex = " ++ show newIndex)
              io (print $ "oldIndex = " ++ show oldIndex)
              (newEnt', newNum) <- if newIndex >= (to^.cfNumEntities)
                                     then return (fromJust newEnt, 9999)
                                     else do
                                       let idx = ((to^.cfFirstEntity) + newIndex) `mod` numClientEntities
                                       Just newEnt' <- preuse $ svGlobals.svServerStatic.ssClientEntities.ix idx
                                       return (newEnt', newEnt'^.esNumber)

              (oldEnt', oldNum) <- if oldIndex >= fromNumEntites
                                     then return (fromJust oldEnt, 9999)
                                     else do
                                       let idx = ((fromJust from^.cfFirstEntity) + oldIndex) `mod` numClientEntities
                                       Just oldEnt' <- preuse $ svGlobals.svServerStatic.ssClientEntities.ix idx
                                       return (oldEnt', oldEnt'^.esNumber)

              io (print $ "oldNum = " ++ show oldNum ++ " newNum = " ++ show newNum)

              if | newNum == oldNum -> do
                     io (print "newNum == oldNum")
                     -- delta update from old position
                     -- because the force parm is false, this will not result
                     -- in any bytes being emited if the entity has not changed at
                     -- all note that players are always 'newentities', this updates
                     -- their oldorigin always
                     -- and prevents warping
                     MSG.writeDeltaEntity oldEnt' newEnt' sizeBufLens False ((newEnt'^.esNumber) <= maxClientsValue)
                     sendEntities maxClientsValue numClientEntities fromNumEntites (Just oldEnt') (Just newEnt') (oldIndex + 1) (newIndex + 1)

                 | newNum < oldNum -> do
                     io (print "newNum < oldNum")
                     -- this is a new entity, send it from the baseline
                     Just baseline <- preuse $ svGlobals.svServer.sBaselines.ix newNum
                     MSG.writeDeltaEntity baseline newEnt' sizeBufLens True True
                     sendEntities maxClientsValue numClientEntities fromNumEntites (Just oldEnt') (Just newEnt') oldIndex (newIndex + 1)

                 | newNum > oldNum -> do
                     io (print "newNum > oldNum")
                     -- the old entity isn't present in the new message
                     let bits = if oldNum >= 256
                                  then Constants.uRemove .|. Constants.uNumber16 .|. Constants.uMoreBits1
                                  else Constants.uRemove

                     MSG.writeByteI sizeBufLens (bits .&. 255)
                     when (bits .&. 0x0000FF00 /= 0) $
                       MSG.writeByteI sizeBufLens ((bits `shiftR` 8) .&. 255)

                     if bits .&. Constants.uNumber16 /= 0
                       then MSG.writeShort sizeBufLens oldNum
                       else MSG.writeByteI sizeBufLens oldNum

                     sendEntities maxClientsValue numClientEntities fromNumEntites (Just oldEnt') (Just newEnt') (oldIndex + 1) newIndex

{-
- Decides which entities are going to be visible to the client, and copies
- off the playerstat and areabits.
-}
buildClientFrame :: ClientReference -> Quake ()
buildClientFrame (ClientReference clientIdx) = do
    io (print "buildClientFrame")
    Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx
    let Just clEntRef@(EdictReference clEntIdx) = client^.cEdict
    Just clEnt <- preuse $ gameBaseGlobals.gbGEdicts.ix clEntIdx

    case clEnt^.eClient of
      Nothing -> return () -- not in game yet
      Just (GClientReference gClientIdx) -> do
        frameNum <- use $ svGlobals.svServer.sFrameNum
        let frameIdx = frameNum .&. Constants.updateMask
            frame = svGlobals.svServerStatic.ssClients.ix clientIdx.cFrames.ix frameIdx :: Traversal' QuakeState ClientFrameT

        realTime <- use $ svGlobals.svServerStatic.ssRealTime -- save it for ping calc later
        frame.cfSentTime .= realTime

        -- find the client's PVS
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
        io (print (gClient^.gcPlayerState.psViewOffset))
        io (print (gClient^.gcPlayerState.psPMoveState.pmsOrigin))
        let org = (gClient^.gcPlayerState.psViewOffset) + fmap ((* 0.125) . fromIntegral) (gClient^.gcPlayerState.psPMoveState.pmsOrigin)

        leafNum <- CM.pointLeafNum org
        clientArea <- CM.leafArea leafNum
        clientCluster <- CM.leafCluster leafNum

        -- calculate the visible areas
        CM.writeAreaBits (svGlobals.svServerStatic.ssClients.ix clientIdx.cFrames.ix frameIdx.cfAreaBits) clientArea >>= \v ->
          frame.cfAreaBytes .= v

        -- grab the current player_state_t
        frame.cfPlayerState .= (gClient^.gcPlayerState)

        fatPVS org
        clientPHS <- CM.clusterPHS clientCluster

        -- build up the list of visible entities
        frame.cfNumEntities .= 0
        use (svGlobals.svServerStatic.ssNextClientEntities) >>= \v ->
          frame.cfFirstEntity .= v

        numEdicts <- use $ gameBaseGlobals.gbNumEdicts

        io (print "collecting edicts")
        collectEdicts org clientPHS clientArea clEntRef frame 1 numEdicts

  where --collectEdicts :: V3Float -> B.ByteString -> Int -> EdictReference -> Traversal' QuakeState ClientFrameT -> Int -> Int -> Quake ()
        collectEdicts org clientPHS clientArea clEntRef frame idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix idx

              io (print $ "working through edict " ++ show idx)

                   -- ignore ents without visible models
              if | (edict^.eSvFlags) .&. Constants.svfNoClient /= 0 -> do
                     io (print "skip svFlags")
                     collectEdicts org clientPHS clientArea clEntRef frame (idx + 1) maxIdx
                   -- ignore ents without visible models unless they have an effect
                 | (edict^.eEntityState.esModelIndex) == 0 && (edict^.eEntityState.esEffects) == 0 && (edict^.eEntityState.esSound) == 0 && (edict^.eEntityState.esEvent) == 0 -> do
                     io (print "skip modelIndex or effects or ...")
                     collectEdicts org clientPHS clientArea clEntRef frame (idx + 1) maxIdx
                 | otherwise -> do
                     -- ignore if not touching a PV leaf
                     -- check area
                     skip <- if EdictReference idx /= clEntRef
                               then do
                                 blocked <- isBlockedByDoor clientArea edict

                                 if blocked
                                   then do
                                     io (print "skip door blocked")
                                     return True
                                   else
                                     -- beams just check one point for PHS
                                     if (edict^.eEntityState.esRenderFx) .&. Constants.rfBeam /= 0
                                       then do
                                         let l = (edict^.eClusterNums) UV.! 0
                                         if (clientPHS `B.index` (l `shiftR` 3)) .&. (1 `shiftL` (l .&. 7)) == 0
                                           then do
                                             io (print "skip beam PHS")
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
                                                     io (print $ "headnode visible = " ++ show visible)
                                                     return (not visible)
                                                   else do -- check individual leafs
                                                     let visible = checkBitVector edict bitVector 0 (edict^.eNumClusters)
                                                     io (print $ "bitvector visible = " ++ show visible)
                                                     return (not visible)

                                         if done
                                           then do
                                             io (print "skip headnode visible")
                                             return True
                                           else
                                             if (edict^.eEntityState.esModelIndex) == 0 -- don't send sounds if they will be attenuated away
                                               then do
                                                 let delta = org - (edict^.eEntityState.esOrigin)
                                                     len = norm delta

                                                 if len > 400
                                                   then do
                                                     io (print "skip len")
                                                     return True
                                                   else return False
                                               else
                                                 return False
                               else
                                 return False

                     if skip
                       then do
                         io (print "skip because SKIP")
                         collectEdicts org clientPHS clientArea clEntRef frame (idx + 1) maxIdx
                       else do
                         io (print "not skipped")
                         serverStatic <- use $ svGlobals.svServerStatic
                         let index = (serverStatic^.ssNextClientEntities) `mod` (serverStatic^.ssNumClientEntities)
                             -- state = (serverStatic^.ssClientEntities) V.! index
                        
                         when ((edict^.eEntityState.esNumber) /= idx) $ do
                           Com.dprintf "FIXING ENT.S.NUMBER!!!\n"
                           gameBaseGlobals.gbGEdicts.ix idx.eEntityState.esNumber .= idx

                         preuse (gameBaseGlobals.gbGEdicts.ix idx.eEntityState) >>= \(Just entityState) ->
                           svGlobals.svServerStatic.ssClientEntities.ix index .= entityState

                         -- don't mark players missiles as solid
                         preuse (svGlobals.svServerStatic.ssClients.ix clientIdx) >>= \(Just client) ->
                           when ((edict^.eOwner) == (client^.cEdict)) $
                             svGlobals.svServerStatic.ssClientEntities.ix index.esSolid .= 0

                         svGlobals.svServerStatic.ssNextClientEntities += 1
                         frame.cfNumEntities += 1

                         collectEdicts org clientPHS clientArea clEntRef frame (idx + 1) maxIdx

        isBlockedByDoor :: Int -> EdictT -> Quake Bool
        isBlockedByDoor clientArea edict = do
          io (print $ "clientArea = " ++ show clientArea)
          io (print $ "areanum = " ++ show (edict^.eAreaNum))
          io (print $ "areanum2 = " ++ show (edict^.eAreaNum2))
          connected <- CM.areasConnected clientArea (edict^.eAreaNum)
          if not connected
            then
              if (edict^.eAreaNum2) == 0
                then return True
                else do
                  connected2 <- CM.areasConnected clientArea (edict^.eAreaNum2)
                  if not connected2
                    then return True
                    else return False
            else
              return False

        checkBitVector :: EdictT -> UV.Vector Word8 -> Int -> Int -> Bool
        checkBitVector edict bitVector idx maxIdx
          | idx >= maxIdx = False
          | otherwise =
              let l = (edict^.eClusterNums) UV.! idx
              in if (bitVector UV.! (l `shiftR` 3)) .&. (1 `shiftL` (l .&. 7)) /= 0
                   then True
                   else checkBitVector edict bitVector (idx + 1) maxIdx

{-
- The client will interpolate the view position, so we can't use a single
- PVS point. 
-}
fatPVS :: V3 Float -> Quake ()
fatPVS org = do
    io (print org)
    let mins = fmap (subtract 8) org
        maxs = fmap (+ 8) org

    (count, _) <- CM.boxLeafNums mins maxs (svGlobals.svLeafsTmp) 64 Nothing
    io (print $ "count = " ++ show count)

    when (count < 1) $
      Com.comError Constants.errFatal "SV_FatPVS: count < 1"

    numClusters <- use $ cmGlobals.cmNumClusters
    let longs = (numClusters + 31) `shiftR` 5
    io (print $ "longs = " ++ show longs)

    -- convert leafs to clusters
    leafs <- use $ svGlobals.svLeafsTmp
    leafs' <- UV.generateM count (\idx -> CM.leafCluster (leafs UV.! idx))

    pvs <- CM.clusterPVS (leafs' UV.! 0)
    fatPVS <- use $ svGlobals.svFatPVS

    -- System.arraycopy(CM.CM_ClusterPVS(leafs[0]), 0, SV_ENTS.fatpvs, 0, longs << 2);
    let updatedFatPVS = UV.generate (UV.length fatPVS) (updateFatPVS pvs fatPVS (longs * 4))

    newFatPVS <- calcNewFatPVS updatedFatPVS leafs' longs 1 count

    svGlobals.svFatPVS .= newFatPVS

  where updateFatPVS :: B.ByteString -> UV.Vector Word8 -> Int -> Int -> Word8
        updateFatPVS pvs fatPVS longs idx =
          if idx < longs
            then pvs `B.index` idx
            else fatPVS UV.! idx

        calcNewFatPVS :: UV.Vector Word8 -> UV.Vector Int -> Int -> Int -> Int -> Quake (UV.Vector Word8)
        calcNewFatPVS fatPVS leafs longs i count
          | i >= count = return fatPVS
          | otherwise = do
              let j = getJ leafs 0 i

              if j /= i
                then calcNewFatPVS fatPVS leafs longs (i + 1) count -- already have the cluster we want
                else do
                  src <- CM.clusterPVS (leafs UV.! i)

                  let updates = constructFatPVSUpdates fatPVS src 0 0 longs []

                  calcNewFatPVS (fatPVS UV.// updates) leafs longs (i + 1) count

        getJ :: UV.Vector Int -> Int -> Int -> Int
        getJ leafs i j
          | j >= i = j
          | otherwise =
              if (leafs UV.! i) == (leafs UV.! j)
                then j
                else getJ leafs i (j + 1)

        constructFatPVSUpdates :: UV.Vector Word8 -> B.ByteString -> Int -> Int -> Int -> [(Int, Word8)] -> [(Int, Word8)]
        constructFatPVSUpdates fatPVS src k j longs acc
          | j >= longs = acc
          | otherwise =
              constructFatPVSUpdates fatPVS src (k + 4) (j + 1) longs ((k + 0, (fatPVS UV.! (k + 0)) .|. (src `B.index` (k + 0))) 
                                                                     : (k + 1, (fatPVS UV.! (k + 1)) .|. (src `B.index` (k + 1)))
                                                                     : (k + 2, (fatPVS UV.! (k + 2)) .|. (src `B.index` (k + 2)))
                                                                     : (k + 3, (fatPVS UV.! (k + 3)) .|. (src `B.index` (k + 3))) 
                                                                     : acc)
