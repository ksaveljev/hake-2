{-# LANGUAGE OverloadedStrings #-}
module Game.GameChase where

import Control.Lens (use, (^.), (.=), zoom, ix, preuse, (%=), (&), (+~), (%~))
import Control.Monad (unless, liftM)
import Data.Bits ((.&.), complement)
import Linear (_x, _z, normalize)

import Game.PlayerStateT
import Game.GClientT
import Game.ClientRespawnT
import Types
import Game.PMoveStateT
import QuakeState
import QuakeRef
import CVarVariables
import qualified Util.Math3D as Math3D

getChaseTarget :: Ref EdictT -> Quake ()
getChaseTarget edictRef = do
    maxClientsValue <- liftM (^.cvValue) maxClientsCVar
    done <- findChaseTarget 1 (truncate maxClientsValue)

    unless done $ do
      centerPrintf <- use $ gameBaseGlobals.gbGameImport.giCenterPrintf
      centerPrintf edictRef "No other players to chase."

  where findChaseTarget :: Int -> Int -> Quake Bool
        findChaseTarget idx maxIdx
          | idx > maxIdx = return False
          | otherwise = do
              let otherRef = Ref idx
              other <- readRef otherRef
              let Just (Ref gClientIdx) = other^.eClient
              Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

              if (other^.eInUse) && not (gClient^.gcResp.crSpectator)
                then do
                  zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
                    gcChaseTarget .= Just otherRef
                    gcUpdateChase .= True

                  updateChaseCam edictRef
                  return True

                else
                  findChaseTarget (idx + 1) maxIdx

updateChaseCam :: Ref EdictT -> Quake ()
updateChaseCam edictRef = do
    edict <- readRef edictRef
    let Just gClientRef@(Ref gClientIdx) = edict^.eClient

    -- is our chase target gone?
    gone <- isChaseTargetGone gClientRef

    unless gone $ do
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      let Just targRef = gClient^.gcChaseTarget
      targ <- readRef targRef
      let Just (Ref targClientIdx) = targ^.eClient
      Just targClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix targClientIdx

      let ownerV = (targ^.eEntityState.esOrigin) & _z +~ fromIntegral (targ^.eViewHeight)
          oldGoal = edict^.eEntityState.esOrigin
          angles = (targClient^.gcVAngle) & _x %~ (\v -> if v > 56 then 56 else v) -- IMPROVE: use Constants.pitch instead of _x directly
          (Just fwrd, Just right, _) = Math3D.angleVectors angles True True False
          forward = normalize fwrd
          o = ownerV + fmap (* (-30)) forward
      io (putStrLn "GameChase.updateChaseCam") >> undefined -- TODO

  where isChaseTargetGone :: Ref GClientT -> Quake Bool
        isChaseTargetGone (Ref gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          let Just chaseTargetRef = gClient^.gcChaseTarget
          chaseTarget <- readRef chaseTargetRef
          let Just (Ref chaseTargetClientIdx) = chaseTarget^.eClient
          Just chaseTargetClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          if not (chaseTarget^.eInUse) || (chaseTargetClient^.gcResp.crSpectator)
            then do
              let oldRef = chaseTargetRef
              chaseNext edictRef
              Just gClient' <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

              if (gClient'^.gcChaseTarget) == Just oldRef
                then do
                  zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
                    gcChaseTarget .= Nothing
                    gcPlayerState.psPMoveState.pmsPMFlags %= (.&. (complement pmfNoPrediction))

                  return True

                else
                  return False

            else
              return False

chaseNext :: Ref EdictT -> Quake ()
chaseNext _ = do
    io (putStrLn "GameChase.chaseNext") >> undefined -- TODO
