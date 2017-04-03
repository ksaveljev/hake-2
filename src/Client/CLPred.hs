{-# LANGUAGE OverloadedStrings #-}
module Client.CLPred where

import Control.Lens (use, preuse, ix, (^.), (.=), (%=))
import Control.Monad (liftM, unless, when)
import Data.Bits ((.&.), shiftR, (.|.))
import Linear (V3(..), _x, _y, _z)

import QCommon.NetChanT
import Game.PlayerStateT
import Client.FrameT
import Types
import Game.PMoveStateT
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.PMoveT as PMoveT
import qualified QCommon.CM as CM
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.PMove as PMove
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

{-
- ================= CL_PredictMovement =================
- 
- Sets cl.predicted_origin and cl.predicted_angles
-}
predictMovement :: Quake ()
predictMovement = do
    state <- use $ globals.gCls.csState
    pausedValue <- liftM (^.cvValue) clPausedCVar

    unless (state /= Constants.caActive || pausedValue /= 0) $ do
      -- io $ print "PREDICT MOVEMENT"
      predictValue <- liftM (^.cvValue) clPredictCVar
      flags <- use $ globals.gCl.csFrame.fPlayerState.psPMoveState.pmsPMFlags

      if predictValue == 0 || (flags .&. PMoveT.pmfNoPrediction) /= 0
        then do
          -- just set angles
          -- io $ print "JUST SET ANGLES"
          viewAngles <- use $ globals.gCl.csViewAngles
          deltaAngles <- use $ globals.gCl.csFrame.fPlayerState.psPMoveState.pmsDeltaAngles
          globals.gCl.csPredictedAngles .= viewAngles + (fmap (Math3D.shortToAngle. fromIntegral) deltaAngles)
        else do
          ack <- use $ globals.gCls.csNetChan.ncIncomingAcknowledged
          current <- use $ globals.gCls.csNetChan.ncOutgoingSequence
          
          -- if we are too far out of date, just freeze
          if current - ack >= Constants.cmdBackup
            then do
              showMiss <- liftM (^.cvValue) clShowMissCVar
              when (showMiss /= 0) $
                Com.printf "exceeded CMD_BACKUP\n"
            else do
              -- io $ print "CALC ANGLES"
              -- copy current state to pmove
              playerPMove <- use $ globals.gCl.csFrame.fPlayerState.psPMoveState
              
              let pm = newPMoveT { _pmTrace = predPMTrace
                                 , _pmPointContents = predPMPointContents
                                 , _pmState = playerPMove
                                 }

              -- io $ print ("viewangles1 = " ++ show (pm^.pmViewAngles))

              Just airAccel <- preuse $ globals.gCl.csConfigStrings.ix Constants.csAirAccel
              pMoveGlobals.pmAirAccelerate .= Lib.atof airAccel
              
              -- run frames
              pm' <- runFrames pm (ack + 1) current

              -- io $ print ("viewangles2 = " ++ show (pm'^.pmViewAngles))

              let oldFrame = (current - 2) .&. (Constants.cmdBackup - 1)
              Just oldZ <- preuse $ globals.gCl.csPredictedOrigins.ix oldFrame._z
              let step = (pm'^.pmState.pmsOrigin._z) - oldZ

              when (step > 63 && step < 160 && ((pm'^.pmState.pmsPMFlags) .&. pmfOnGround /= 0)) $ do
                realTime <- use $ globals.gCls.csRealTime
                frameTime <- use $ globals.gCls.csFrameTime
                globals.gCl.csPredictedStep .= fromIntegral step * 0.125
                globals.gCl.csPredictedStepTime .= truncate (fromIntegral realTime - frameTime * 500)

              -- copy results out for rendering
              globals.gCl.csPredictedOrigin .= fmap ((* 0.125) . fromIntegral) (pm'^.pmState.pmsOrigin)
              globals.gCl.csPredictedAngles .= (pm'^.pmViewAngles)

  where runFrames :: PMoveT -> Int -> Int -> Quake PMoveT
        runFrames pm ack current
          | ack >= current = return pm
          | otherwise = do
              let frame = ack .&. (Constants.cmdBackup - 1)
              Just cmd <- preuse $ globals.gCl.csCmds.ix frame

              pm' <- PMove.pMove pm { _pmCmd = cmd }

              -- save for debug checking
              globals.gCl.csPredictedOrigins.ix frame .= (pm'^.pmState.pmsOrigin)

              runFrames pm' (ack + 1) current

predPMTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake (Maybe TraceT)
predPMTrace start mins maxs end = do
    -- check against world
    t <- CM.boxTrace start end mins maxs 0 Constants.maskPlayerSolid

    let t' = if (t^.tFraction) < 1
               -- TODO: do not forget about this dummy edict
               then t { _tEnt = Just (Ref (Constants.maxEdicts)) } -- dummy ent
               else t

    -- check all other solid models
    liftM Just $ clipMoveToEntities start mins maxs end t'

clipMoveToEntities :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> TraceT -> Quake TraceT
clipMoveToEntities start mins maxs end tr = do
    numEntities <- use $ globals.gCl.csFrame.fNumEntities
    clipMoveToEntity tr 0 numEntities

  where clipMoveToEntity :: TraceT -> Int -> Int -> Quake TraceT
        clipMoveToEntity traceT idx maxIdx
          | idx >= maxIdx = return traceT
          | otherwise = do
              parseEntities <- use $ globals.gCl.csFrame.fParseEntities
              let num = (parseEntities + idx) .&. (Constants.maxParseEntities - 1)
              Just ent <- preuse $ globals.gClParseEntities.ix num
              playerNum <- use $ globals.gCl.csPlayerNum

              if (ent^.esSolid) == 0 || (ent^.esNumber) == playerNum + 1
                then
                  clipMoveToEntity traceT (idx + 1) maxIdx
                else do
                  result <- if (ent^.esSolid) == 31 -- special value for bmodel
                              then do
                                Just maybeCModel <- preuse $ globals.gCl.csModelClip.ix (ent^.esModelIndex)

                                case maybeCModel of
                                  Nothing -> return Nothing
                                  Just (Ref modelIdx) -> do
                                    Just model <- preuse $ cmGlobals.cmMapCModels.ix modelIdx
                                    return (Just (model^.cmHeadNode, ent^.esAngles))

                              else do
                                let x = 8 * ((ent^.esSolid) .&. 31)
                                    zd = 8 * (((ent^.esSolid) `shiftR` 5) .&. 31)
                                    zu = 8 * (((ent^.esSolid) `shiftR` 10) .&. 63) - 32
                                    bmins = V3 (-x) (-x) (-zd)
                                    bmaxs = V3 x x zu

                                headNode <- CM.headnodeForBox (fmap fromIntegral bmins) (fmap fromIntegral bmaxs)
                                angles <- use $ globals.gVec3Origin -- boxes don't rotate
                                return (Just (headNode, angles))

                  case result of
                    Nothing ->
                      clipMoveToEntity traceT (idx + 1) maxIdx
                    Just (headNode, angles) -> do
                      if (traceT^.tAllSolid)
                        then
                          clipMoveToEntity traceT (idx + 1) maxIdx
                        else do
                          traceT' <- CM.transformedBoxTrace start end mins maxs headNode Constants.maskPlayerSolid (ent^.esOrigin) angles

                          if (traceT'^.tAllSolid) || (traceT'^.tStartSolid) || (traceT'^.tFraction) < (traceT^.tFraction)
                            then do
                              if (traceT^.tStartSolid)
                                then do
                                  clipMoveToEntity (traceT' { _tEnt = (ent^.esSurroundingEnt), _tStartSolid = True}) (idx + 1) maxIdx
                                else do
                                  clipMoveToEntity (traceT' { _tEnt = (ent^.esSurroundingEnt)}) (idx + 1) maxIdx
                            else
                              clipMoveToEntity (traceT { _tStartSolid = True }) (idx + 1) maxIdx

{-
- ================= PMpointcontents
- 
- Returns the content identificator of the point. =================
-}
predPMPointContents :: V3 Float -> Quake Int
predPMPointContents point = do
    contents <- CM.pointContents point 0

    numEntities <- use $ globals.gCl.csFrame.fNumEntities

    calcContents contents 0 numEntities

  where calcContents :: Int -> Int -> Int -> Quake Int
        calcContents contents idx maxIdx
          | idx >= maxIdx = return contents
          | otherwise = do
              parseEntities <- use $ globals.gCl.csFrame.fParseEntities
              let num = (parseEntities + idx) .&. (Constants.maxParseEntities - 1)
              Just ent <- preuse $ globals.gClParseEntities.ix num

              if (ent^.esSolid) /= 31 -- special value for bmodel
                then
                  calcContents contents (idx + 1) maxIdx
                else do
                  Just maybeCModel <- preuse $ globals.gCl.csModelClip.ix (ent^.esModelIndex)

                  case maybeCModel of
                    Nothing -> calcContents contents (idx + 1) maxIdx
                    Just (Ref modelIdx) -> do
                      Just model <- preuse $ cmGlobals.cmMapCModels.ix modelIdx
                      v <- CM.transformedPointContents point (model^.cmHeadNode) (ent^.esOrigin) (ent^.esAngles)
                      calcContents (contents .|. v) (idx + 1) maxIdx

checkPredictionError :: Quake ()
checkPredictionError = do
    predictValue <- liftM (^.cvValue) clPredictCVar
    flags <- use $ globals.gCl.csFrame.fPlayerState.psPMoveState.pmsPMFlags

    unless (predictValue == 0 || (flags .&. pmfNoPrediction) /= 0) $ do
      -- calculate the last usercmd_t we sent that the server has processed
      incomingAcknowledged <- use $ globals.gCls.csNetChan.ncIncomingAcknowledged
      let frame = incomingAcknowledged .&. (Constants.cmdBackup - 1)

      -- compare what the server returned with what we had predicted it to be
      origin <- use $ globals.gCl.csFrame.fPlayerState.psPMoveState.pmsOrigin
      Just predictedOrigin <- preuse $ globals.gCl.csPredictedOrigins.ix frame
      let delta = origin - predictedOrigin

      -- save the prediction error for interpolation
      let len = abs (delta^._x) + abs (delta^._y) + abs (delta^._z)

      if len > 640 -- 80 world units
        then -- a teleport or something
          globals.gCl.csPredictionError .= V3 0 0 0
        else do
          -- TODO: show prediction miss here! (not implemented)

          globals.gCl.csPredictedOrigins.ix frame .= origin

          -- save for error interpolation
          globals.gCl.csPredictionError .= fmap ((* 0.125) . fromIntegral) delta
