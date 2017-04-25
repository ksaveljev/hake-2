{-# LANGUAGE FlexibleContexts #-}
module Client.CLPred
    ( checkPredictionError
    , predictMovement
    ) where

import           Control.Lens          (preuse, use, ix, (^.), (.=), (&), (.~))
import           Control.Monad         (when, unless)
import           Data.Bits             (shiftR, (.&.), (.|.))
import           Data.Int              (Int8, Int16)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as UV
import           Linear                (V3(..), _x, _y, _z)

import           Client.ClientStateT
import           Client.ClientStaticT
import           Client.FrameT
import qualified Constants
import           Game.CModelT
import           Game.CVarT
import           Game.EntityStateT
import           Game.PlayerStateT
import           Game.PMoveStateT
import           Game.PMoveT
import           Game.TraceT
import qualified QCommon.Com           as Com
import qualified QCommon.CM            as CM
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
        cmd <- readRef (Ref frame)
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

predPMTrace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake TraceT
predPMTrace start mins maxs end = do
    -- check against world
    t <- CM.boxTrace start end mins maxs 0 Constants.maskPlayerSolid
    let t' = if (t^.tFraction) < 1 then t & tEnt .~ Just (Ref Constants.maxEdicts) else t -- don't forget about this dummy edict
    clipMoveToEntities start mins maxs end t'

clipMoveToEntities :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> TraceT -> Quake TraceT
clipMoveToEntities start mins maxs end t = do
    numEntities <- use (globals.gCl.csFrame.fNumEntities)
    doClipMoveToEntities t 0 numEntities
  where
    doClipMoveToEntities traceT idx maxIdx
        | idx >= maxIdx = return traceT
        | otherwise = do
            parseEntities <- use (globals.gCl.csFrame.fParseEntities)
            playerNum <- use (globals.gCl.csPlayerNum)
            entityStates <- use (globals.gClParseEntities)
            let num = (parseEntities + idx) .&. (Constants.maxParseEntities - 1)
            proceedClipMoveToEntities traceT (entityStates V.! num) playerNum idx maxIdx
    proceedClipMoveToEntities traceT entityState playerNum idx maxIdx
        | (entityState^.esSolid) == 0 || (entityState^.esNumber) == (playerNum + 1) =
            doClipMoveToEntities traceT (idx + 1) maxIdx
        | otherwise = do
            headNodeAndAngles <- getHeadNodeAndAngles entityState
            maybe (doClipMoveToEntities traceT (idx + 1) maxIdx)
                  (continueClipMoveToEntities traceT entityState idx maxIdx)
                  headNodeAndAngles
    getHeadNodeAndAngles entityState
        | (entityState^.esSolid) == 31 = do -- special value for bmodel
            modelClips <- use (globals.gCl.csModelClip)
            maybe (return Nothing) (getModelHeadNodeAndAngles entityState) (modelClips V.! (entityState^.esModelIndex))
        | otherwise = do
            let x = 8 * ((entityState^.esSolid) .&. 31)
                zd = 8 * (((entityState^.esSolid) `shiftR` 5) .&. 31)
                zu = 8 * (((entityState^.esSolid) `shiftR` 10) .&. 63) - 32
                bmins = V3 (-x) (-x) (-zd)
                bmaxs = V3 x x zu
            headNode <- CM.headnodeForBox (fmap fromIntegral bmins) (fmap fromIntegral bmaxs)
            angles <- use (globals.gVec3Origin) -- boxes don't rotate
            return (Just (headNode, angles))
    getModelHeadNodeAndAngles entityState modelRef = do
        model <- readRef modelRef
        return (Just (model^.cmHeadNode, entityState^.esAngles))
    continueClipMoveToEntities traceT entityState idx maxIdx (headNode, angles)
        | (traceT^.tAllSolid) = return traceT
        | otherwise = do
            traceT' <- CM.transformedBoxTrace start end mins maxs headNode Constants.maskPlayerSolid (entityState^.esOrigin) angles
            doClipMoveToEntities (updateTraceT entityState traceT traceT') (idx + 1) maxIdx
    updateTraceT entityState traceT traceT'
        | (traceT'^.tAllSolid) || (traceT'^.tStartSolid) || (traceT'^.tFraction) < (traceT^.tFraction) =
            traceT' & tEnt .~ (entityState^.esSurroundingEnt) -- TODO make sure this one is ok (jake2 has some copy mechanism here which doesn't copy everything from TraceT to TraceT
        | otherwise = traceT & tStartSolid .~ True

predPMPointContents :: V3 Float -> Quake Int
predPMPointContents point = do
    contents <- CM.pointContents point 0
    numEntities <- use (globals.gCl.csFrame.fNumEntities)
    calcContents point contents 0 numEntities

calcContents :: V3 Float -> Int -> Int -> Int -> Quake Int
calcContents point contents idx maxIdx
    | idx >= maxIdx = return contents
    | otherwise = do
        parseEntities <- use (globals.gCl.csFrame.fParseEntities)
        entityState <- getEntityState parseEntities
        doCalcContents entityState
  where
    getEntityState parseEntities = do
        entityStates <- use (globals.gClParseEntities)
        return (entityStates V.! ((parseEntities + idx) .&. (Constants.maxParseEntities - 1)))
    doCalcContents entityState
        | (entityState^.esSolid) /= 31 = -- special value for bmodel
            calcContents point contents (idx + 1) maxIdx
        | otherwise = do
            modelClips <- use (globals.gCl.csModelClip)
            maybe (calcContents point contents (idx + 1) maxIdx)
                  (proceedCalcContents entityState)
                  (modelClips V.! (entityState^.esModelIndex))
    proceedCalcContents entityState modelRef = do
        model <- readRef modelRef
        v <- CM.transformedPointContents point (model^.cmHeadNode) (entityState^.esOrigin) (entityState^.esAngles)
        calcContents point (contents .|. v) (idx + 1) maxIdx

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
