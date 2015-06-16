{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Server.SVEnts where

import Control.Lens (use, Lens', (^.), preuse, ix, Traversal', (.=))
import Control.Monad (when)
import Data.Bits ((.&.), shiftR)
import Data.Maybe (isJust)
import Linear (V3)
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

        collectEdicts clEntRef 1 numEdicts

  where collectEdicts :: EdictReference -> Int -> Int -> Quake ()
        collectEdicts clEntRef idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix idx

                   -- ignore ents without visible models
              if | (edict^.eSvFlags) .&. Constants.svfNoClient /= 0 -> collectEdicts clEntRef (idx + 1) maxIdx
                   -- ignore ents without visible models unless they have an effect
                 | (edict^.eEntityState.esModelIndex) == 0 && (edict^.eEntityState.esEffects) == 0 && (edict^.eEntityState.esSound) == 0 && (edict^.eEntityState.esEvent) == 0 -> collectEdicts clEntRef (idx + 1) maxIdx
                 | otherwise -> do
                     -- ignore if not touching a PV leaf
                     -- check area
                     skip <- if (EdictReference idx) /= clEntRef
                               then do
                                 io (putStrLn "SVEnts.buildClientFrame") >> undefined -- TODO
                               else
                                 return False

                     if skip
                       then collectEdicts clEntRef (idx + 1) maxIdx
                       else do
                         undefined -- TODO

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
    clusters <- UV.generateM count (\idx -> CM.leafCluster (leafs UV.! idx))

    io (putStrLn "SVEnts.fatPVS") >> undefined -- TODO
