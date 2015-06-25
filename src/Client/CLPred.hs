{-# LANGUAGE OverloadedStrings #-}
module Client.CLPred where

import Control.Lens (use, preuse, ix, (^.), (.=), (%=))
import Control.Monad (liftM, unless, when)
import Data.Bits ((.&.))
import Linear (V3(..), _x, _y, _z)

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.PMoveT as PMoveT
import qualified QCommon.Com as Com
import qualified Util.Math3D as Math3D

{-
- ================= CL_PredictMovement =================
- 
- Sets cl.predicted_origin and cl.predicted_angles
-}
predictMovement :: Quake ()
predictMovement = do
    state <- use $ globals.cls.csState
    pausedValue <- liftM (^.cvValue) clPausedCVar

    unless (state /= Constants.caActive || pausedValue /= 0) $ do
      predictValue <- liftM (^.cvValue) clPredictCVar
      flags <- use $ globals.cl.csFrame.fPlayerState.psPMoveState.pmsPMFlags

      if predictValue == 0 || (fromIntegral flags .&. PMoveT.pmfNoPrediction) /= 0
        then do
          -- just set angles
          viewAngles <- use $ globals.cl.csViewAngles
          deltaAngles <- use $ globals.cl.csFrame.fPlayerState.psPMoveState.pmsDeltaAngles
          globals.cl.csPredictedAngles .= viewAngles + (fmap (Math3D.shortToAngle. fromIntegral) deltaAngles)
        else do
          ack <- use $ globals.cls.csNetChan.ncIncomingAcknowledged
          current <- use $ globals.cls.csNetChan.ncOutgoingSequence
          
          -- if we are too far out of date, just freeze
          if current - ack >= Constants.cmdBackup
            then do
              showMiss <- liftM (^.cvValue) clShowMissCVar
              when (showMiss /= 0) $
                Com.printf "exceeded CMD_BACKUP\n"
            else do
              -- copy current state to pmove
              io (putStrLn "CLPred.predictMovement") >> undefined -- TODO

checkPredictionError :: Quake ()
checkPredictionError = do
    predictValue <- liftM (^.cvValue) clPredictCVar
    flags <- use $ globals.cl.csFrame.fPlayerState.psPMoveState.pmsPMFlags

    unless (predictValue == 0 || (fromIntegral flags .&. pmfNoPrediction) /= 0) $ do
      -- calculate the last usercmd_t we sent that the server has processed
      globals.cls.csNetChan.ncIncomingAcknowledged %= (.&. (Constants.cmdBackup - 1))
      frame <- use $ globals.cls.csNetChan.ncIncomingAcknowledged

      -- compare what the server returned with what we had predicted it to be
      origin <- use $ globals.cl.csFrame.fPlayerState.psPMoveState.pmsOrigin
      Just predictedOrigin <- preuse $ globals.cl.csPredictedOrigins.ix frame
      let delta = origin - predictedOrigin

      -- save the prediction error for interpolation
      let len = abs (delta^._x) + abs (delta^._y) + abs (delta^._z)

      if len > 640 -- 80 world units
        then -- a teleport or something
          globals.cl.csPredictionError .= V3 0 0 0
        else do
          -- TODO: show prediction miss here! (not implemented)

          globals.cl.csPredictedOrigins.ix frame .= origin

          -- save for error interpolation
          globals.cl.csPredictionError .= fmap ((* 0.125) . fromIntegral) delta
