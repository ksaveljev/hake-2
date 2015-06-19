{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Server.SVEnts where

import Control.Lens (use, Lens', (^.), preuse, ix, Traversal', (.=), (+=))
import Control.Monad (when)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Linear (V3, _x, _y, _z, _w)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
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
    when (pflags .&. Constants.psMType /= 0) $
      MSG.writeByteI sizeBufLens (ps^.psPMoveState.pmsPMType)

    when (pflags .&. Constants.psMOrigin /= 0) $ do
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsOrigin._x)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsOrigin._y)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsOrigin._z)

    when (pflags .&. Constants.psMVelocity /= 0) $ do
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsVelocity._x)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsVelocity._y)
      MSG.writeShort sizeBufLens (fromIntegral $ ps^.psPMoveState.pmsVelocity._z)

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

    -- write the rest of the player_state_t
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

    when (pflags .&. Constants.psWeaponIndex /= 0) $ do
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

emitPacketEntities :: Maybe ClientFrameT -> ClientFrameT -> Lens' QuakeState SizeBufT -> Quake ()
emitPacketEntities _ _ _ = do
    io (putStrLn "SVEnts.emitPacketEntities") >> undefined -- TODO

{-
- Decides which entities are going to be visible to the client, and copies
- off the playerstat and areabits.
-}
buildClientFrame :: ClientReference -> Quake ()
buildClientFrame (ClientReference clientIdx) = do
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

        collectEdicts clEntRef frame 1 numEdicts

  where --collectEdicts :: EdictReference -> Traversal' QuakeState ClientFrameT -> Int -> Int -> Quake ()
        collectEdicts clEntRef frame idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix idx

                   -- ignore ents without visible models
              if | (edict^.eSvFlags) .&. Constants.svfNoClient /= 0 -> collectEdicts clEntRef frame (idx + 1) maxIdx
                   -- ignore ents without visible models unless they have an effect
                 | (edict^.eEntityState.esModelIndex) == 0 && (edict^.eEntityState.esEffects) == 0 && (edict^.eEntityState.esSound) == 0 && (edict^.eEntityState.esEvent) == 0 -> collectEdicts clEntRef frame (idx + 1) maxIdx
                 | otherwise -> do
                     -- ignore if not touching a PV leaf
                     -- check area
                     skip <- if (EdictReference idx) /= clEntRef
                               then do
                                 io (putStrLn "SVEnts.buildClientFrame") >> undefined -- TODO
                               else
                                 return False

                     if skip
                       then collectEdicts clEntRef frame (idx + 1) maxIdx
                       else do
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

{-
- The client will interpolate the view position, so we can't use a single
- PVS point. 
-}
fatPVS :: V3 Float -> Quake ()
fatPVS org = do
    let mins = fmap (subtract 8) org
        maxs = fmap (+ 8) org

    (count, _) <- CM.boxLeafNums mins maxs (svGlobals.svLeafsTmp) 64 Nothing

    when (count < 1) $
      Com.comError Constants.errFatal "SV_FatPVS: count < 1"

    numClusters <- use $ cmGlobals.cmNumClusters
    let longs = (numClusters + 31) `shiftR` 5

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
