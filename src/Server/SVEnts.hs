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
import qualified Data.Vector              as V
import qualified Data.Vector.Storable     as SV
import qualified Data.Vector.Unboxed      as UV
import           Linear                   (V3(..), norm)

import qualified Constants
import           Game.EdictT
import           Game.EntityStateT
import           Game.GClientT
import           Game.PlayerStateT
import           Game.PMoveStateT
import qualified QCommon.CM               as CM
import qualified QCommon.Com              as Com
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
writePlayerStateToClient = error "SVEnts.writePlayerStateToClient" -- TODO

emitPacketEntities :: Maybe ClientFrameT -> ClientFrameT -> Lens' QuakeState SizeBufT -> Quake ()
emitPacketEntities = error "SVEnts.emitPacketEntities" -- TODO

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
