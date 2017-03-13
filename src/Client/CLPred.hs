{-# LANGUAGE FlexibleContexts #-}
module Client.CLPred
    ( checkPredictionError
    , predictMovement
    ) where

import           Control.Lens          (preuse, use, ix, (^.), (.=), (&), (.~))
import           Control.Monad         (when, unless)
import           Data.Bits             ((.&.))
import           Data.Int              (Int8, Int16)
import qualified Data.Vector.Unboxed   as UV
import           Linear                (V3(..), _x, _y, _z)

import           Client.ClientStateT
import           Client.ClientStaticT
import           Client.FrameT
import qualified Constants
import           Game.CVarT
import           Game.PlayerStateT
import           Game.PMoveStateT
import           Game.PMoveT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QCommon.NetChanT
import qualified QCommon.PMove         as PMove
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

predictMovement :: Quake ()
predictMovement = do
    state <- use (globals.gCls.csState)
    paused <- pausedCVar
    unless (state /= Constants.caActive || (paused^.cvValue) /= 0) $ do
        predict <- clPredictCVar
        flags <- use (globals.gCl.csFrame.fPlayerState.psPMoveState.pmsPMFlags)
        doPredictMovement predict flags

doPredictMovement :: CVarT -> Int8 -> Quake ()
doPredictMovement predict flags
    | (predict^.cvValue) == 0 || (flags .&. Constants.pmfNoPrediction) /= 0 = do
        viewAngles <- use (globals.gCl.csViewAngles)
        deltaAngles <- use (globals.gCl.csFrame.fPlayerState.psPMoveState.pmsDeltaAngles)
        globals.gCl.csPredictedAngles .= viewAngles + (fmap (Math3D.shortToAngle . fromIntegral) deltaAngles)
    | otherwise = do
        ack <- use (globals.gCls.csNetChan.ncIncomingAcknowledged)
        current <- use (globals.gCls.csNetChan.ncOutgoingSequence)
        checkBackupAndPredict ack current

checkBackupAndPredict :: Int -> Int -> Quake ()
checkBackupAndPredict ack current
    | current - ack >= Constants.cmdBackup = do
        showMiss <- clShowMissCVar
        when ((showMiss^.cvValue) /= 0) $
            Com.printf "exceeded CMD_BACKUP\n"
    | otherwise = do
        setAirAccelerate
        pm <- doPredict =<< use (globals.gCl.csFrame.fPlayerState.psPMoveState)
        checkStep current pm
        globals.gCl.csPredictedOrigin .= fmap ((* 0.125) . fromIntegral) (pm^.pmState.pmsOrigin)
        globals.gCl.csPredictedAngles .= (pm^.pmViewAngles)
  where
    setAirAccelerate = do
        airAccel <- preuse (globals.gCl.csConfigStrings.ix Constants.csAirAccel)
        maybe airAccelError doSetAirAccelerate airAccel
    airAccelError = error "CLPred.checkBackup airAccel is Nothing"
    doSetAirAccelerate airAccel =
        pMoveGlobals.pmAirAccelerate .= Lib.atof airAccel
    doPredict playerPMove =
        runFrames (newPMoveT & pmTrace .~ predPMTrace
                             & pmPointContents .~ predPMPointContents
                             & pmState .~ playerPMove)
                  (ack + 1)
                  current

runFrames :: PMoveT -> Int -> Int -> Quake PMoveT
runFrames pm ack current
    | ack >= current = return pm
    | otherwise = do
        cmd <- readRef (Ref Constants.noParent frame)
        pm' <- PMove.pMove (pm & pmCmd .~ cmd)
        globals.gCl.csPredictedOrigins.ix frame .= (pm'^.pmState.pmsOrigin)
        runFrames pm' (ack + 1) current
  where
    frame = ack .&. (Constants.cmdBackup - 1)

checkStep :: Int -> PMoveT -> Quake ()
checkStep current pm = do
    oldZ <- preuse (globals.gCl.csPredictedOrigins.ix oldFrame._z)
    maybe oldZError (doCheckStep pm) oldZ
  where
    oldFrame = (current - 2) .&. (Constants.cmdBackup - 1)
    oldZError = error "CLPred.checkStep oldZ is Nothing"

doCheckStep :: PMoveT -> Int16 -> Quake ()
doCheckStep pm oldZ
    | step > 63 && step < 160 && ((pm^.pmState.pmsPMFlags) .&. Constants.pmfOnGround /= 0) = do
        realTime <- use (globals.gCls.csRealTime)
        frameTime <- use (globals.gCls.csFrameTime)
        globals.gCl.csPredictedStep .= fromIntegral step * 0.125
        globals.gCl.csPredictedStepTime .= truncate (fromIntegral realTime - frameTime * 500)
    | otherwise = return ()
  where
    step = (pm^.pmState.pmsOrigin._z) - oldZ

predPMTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake (Maybe TraceT)
predPMTrace = error "CLPred.predPMTrace" -- TODO

predPMPointContents :: V3 Float -> Quake Int
predPMPointContents = error "CLPred.predPMPointContents" -- TODO

checkPredictionError :: Quake ()
checkPredictionError = do
    predict <- fmap (^.cvValue) clPredictCVar
    flags <- use (globals.gCl.csFrame.fPlayerState.psPMoveState.pmsPMFlags)
    unless (predict == 0 || (flags .&. Constants.pmfNoPrediction) /= 0) $ do
        -- calculate the last usercmd_t we sent that the server has processed
        incomingAcknowledged <- use (globals.gCls.csNetChan.ncIncomingAcknowledged)
        -- compare what the server returned with what we had predicted it to be
        origin <- use (globals.gCl.csFrame.fPlayerState.psPMoveState.pmsOrigin)
        predictedOrigin <- getPredictedOrigin (incomingAcknowledged .&. (Constants.cmdBackup - 1))
        updatePredictionError incomingAcknowledged origin predictedOrigin
 where
    getPredictedOrigin frame = do
        predictedOrigins <- use (globals.gCl.csPredictedOrigins)
        return (predictedOrigins UV.! frame)

updatePredictionError :: Int -> V3 Int16 -> V3 Int16 -> Quake ()
updatePredictionError incomingAcknowledged origin predictedOrigin
    | len > 640 = -- 80 world units
        globals.gCl.csPredictionError .= V3 0 0 0 -- teleport or something
    | otherwise = do
        -- TODO: show prediction miss here! (not implemented)
        globals.gCl.csPredictedOrigins.ix frame .= origin
        -- save for error interpolation
        globals.gCl.csPredictionError .= fmap ((* 0.125) . fromIntegral) delta
  where
    delta = origin - predictedOrigin
    len = abs (delta^._x) + abs (delta^._y) + abs (delta^._z)
    frame = incomingAcknowledged .&. (Constants.cmdBackup - 1)