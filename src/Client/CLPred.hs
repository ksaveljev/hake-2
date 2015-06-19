{-# LANGUAGE OverloadedStrings #-}
module Client.CLPred where

import Control.Lens (use, (^.), (.=))
import Control.Monad (liftM, unless, when)
import Data.Bits ((.&.))

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
          globals.cl.csPredictedAngles .= viewAngles + (fmap Math3D.shortToAngle deltaAngles)
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
    io (putStrLn "CLPred.checkPredictionError") >> undefined -- TODO
