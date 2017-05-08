{-# LANGUAGE Rank2Types #-}
module Server.SVEnts
    ( buildClientFrame
    , recordDemoMessage
    , writeFrameToClient
    ) where

import           Control.Lens         (Lens', Traversal', use, ix, (.=), (+=), (^.), (&), (.~))
import           Control.Monad        (when)
import           Data.Bits            (shiftL, shiftR, (.&.))
import qualified Data.ByteString      as B
import qualified Data.Vector.Unboxed  as UV
import           Linear               (V3(..), norm)

import qualified Constants
import           Game.EdictT
import           Game.EntityStateT
import           Game.GClientT
import           Game.PlayerStateT
import           Game.PMoveStateT
import qualified QCommon.CM           as CM
import qualified QCommon.Com          as Com
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
writeFrameToClient = error "SVEnts.writeFrameToClient" -- TODO

fatPVS :: V3 Float -> Quake ()
fatPVS = error "SVEnts.fatPVS" -- TODO
