{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Server.SVEnts where

import Control.Lens (use, Lens', (^.), preuse, ix, Traversal', (.=), (+=))
import Control.Monad (when)
import Data.Bits ((.&.), (.|.), shiftR)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Linear (V3)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com

{-
- Save everything in the world out without deltas. Used for recording
- footage for merged or assembled demos.
-}
recordDemoMessage :: Quake ()
recordDemoMessage = do
    demoFile <- use $ svGlobals.svServerStatic.ssDemoFile

    when (isJust demoFile) $ do
      io (putStrLn "SVEnts.recordDemoMessage") >> undefined -- TODO

writeFrameToClient :: ClientReference -> Lens' QuakeState SizeBufT -> Quake ()
writeFrameToClient _ _ = do
    io (putStrLn "SVEnts.writeFrameToClient") >> undefined -- TODO

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
