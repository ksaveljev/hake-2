module QCommon.PMove
    ( pMove
    ) where

import           Control.Lens     (use, (^.), (.=), (%=), (&), (.~), (%~))
import           Control.Monad    (when)
import           Data.Bits        (complement, shiftR, (.&.), (.|.))
import           Data.Int         (Int16)
import           Linear           (V3(..), norm, normalize, _x, _y, _z)

import qualified Constants
import           Game.PMoveStateT
import           Game.PMoveT
import           Game.TraceT
import           Game.UserCmdT
import qualified QCommon.Com      as Com
import           QCommon.PmlT
import           QuakeState
import           Types
import qualified Util.Math3D      as Math3D

offset :: V3 Int16
offset = V3 0 (-1) 1

pMove :: PMoveT -> Quake PMoveT
pMove pmove = do
    clearResults pmove
    clampAngles
    proceedPMove pmove

proceedPMove :: PMoveT -> Quake PMoveT
proceedPMove pmove
    | (pmove^.pmState.pmsPMType) == Constants.pmSpectator = do
        flyMove False
        snapPosition
        use (pMoveGlobals.pmPM)
    | otherwise = do
        when ((pmove^.pmState.pmsPMType) >= Constants.pmDead) $
            pMoveGlobals.pmPM.pmCmd %= (\v -> v & ucForwardMove .~ 0
                                                & ucSideMove .~ 0
                                                & ucUpMove .~ 0)
        freezeOrMove
  where
    freezeOrMove
        | (pmove^.pmState.pmsPMType) == Constants.pmFreeze =
            use (pMoveGlobals.pmPM)
        | otherwise = do
            checkDuck =<< use (pMoveGlobals.pmPM)
            when (pmove^.pmSnapInitial) $
                initialSnapPosition
            catagorizePosition
            when ((pmove^.pmState.pmsPMType) == Constants.pmDead) $
                deadMove
            checkSpecialMovement
            dropTimingCounter
            checkPMFlags
            catagorizePosition
            snapPosition
            use (pMoveGlobals.pmPM)

clearResults :: PMoveT -> Quake ()
clearResults pmove = do
    pml <- use (pMoveGlobals.pmPML)
    pMoveGlobals.pmPM .= (pmove & pmNumTouch     .~ 0
                                & pmViewAngles   .~ V3 0 0 0
                                & pmViewHeight   .~ 0
                                & pmGroundEntity .~ Nothing
                                & pmWaterType    .~ 0
                                & pmWaterLevel   .~ 0)
    pMoveGlobals.pmPML .= (pml & pmlGroundSurface  .~ Nothing
                               & pmlGroundContents .~ 0
                               & pmlOrigin         .~ fmap ((* 0.125) . fromIntegral) (pmove^.pmState.pmsOrigin)
                               & pmlVelocity       .~ fmap ((* 0.125) . fromIntegral) (pmove^.pmState.pmsVelocity)
                               & pmlPreviousOrigin .~ fmap fromIntegral (pmove^.pmState.pmsOrigin)
                               & pmlFrameTime      .~ fromIntegral (fromIntegral (pmove^.pmCmd.ucMsec) .&. 0xFF :: Int) * 0.001)

clampAngles :: Quake ()
clampAngles = do
    pMoveGlobals.pmPM %= updatePM
    (forward, right, up) <- getAngleVectors
    pMoveGlobals.pmPML %= (\v -> v & pmlForward .~ forward
                                   & pmlRight .~ right
                                   & pmlUp .~ up)
  where
    updatePM pm
      | (pm^.pmState.pmsPMFlags) .&. Constants.pmfTimeTeleport /= 0 =
          -- IMPROVE: think how to update it using Constants.yaw, Constants.pitch and Constants.roll
          pm & pmViewAngles .~ V3 0 (fromIntegral ((pm^.pmCmd.ucAngles._y) + (pm^.pmState.pmsDeltaAngles._y))) 0
      | otherwise =
          let V3 a b c = fmap Math3D.shortToAngle (fmap fromIntegral ((pm^.pmCmd.ucAngles) + (pm^.pmState.pmsDeltaAngles)))
              a' | a > 89 && a < 180   = 89
                 | a < 271 && a >= 180 = 271
                 | otherwise           = a
          in pm & pmViewAngles .~ V3 a' b c
    getAngleVectors = do
        pm <- use (pMoveGlobals.pmPM)
        return (Math3D.angleVectors (pm^.pmViewAngles) True True True)

checkDuck :: PMoveT -> Quake ()
checkDuck pm
    | (pm^.pmState.pmsPMType) == Constants.pmGib =
        pMoveGlobals.pmPM .= (pm & pmMins .~ V3 minsX minsY 0
                                 & pmMaxs .~ V3 maxsX maxsY 16
                                 & pmViewHeight .~ 8)
    | otherwise =
        duckOrStandUp >>= updatePMove
  where
    minsX = -16
    minsY = -16
    maxsX = 16
    maxsY = 16
    duckOrStandUp
        | (pm^.pmState.pmsPMType) == Constants.pmDead =
            return (pm & pmState.pmsPMFlags .~ (pm^.pmState.pmsPMFlags) .|. Constants.pmfDucked)
        | (pm^.pmCmd.ucUpMove) < 0 && ((pm^.pmState.pmsPMFlags) .&. Constants.pmfOnGround /= 0) = -- duck
            return (pm & pmState.pmsPMFlags .~ (pm^.pmState.pmsPMFlags) .|. Constants.pmfDucked)
        | (pm^.pmState.pmsPMFlags) .&. Constants.pmfDucked /= 0 = do -- stand up if possible
            pml <- use (pMoveGlobals.pmPML)
            traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (V3 (-16) (-16) (-24)) (V3 16 16 32) (pml^.pmlOrigin)
            maybe traceError standUp traceT
        | otherwise =
            return pm
    traceError = do
        Com.fatalError "PMove.checkDuck traceT is Nothing"
        return pm
    standUp traceT
        | traceT^.tAllSolid = return pm
        | otherwise = return (pm & pmState.pmsPMFlags .~ (pm^.pmState.pmsPMFlags) .&. (complement Constants.pmfDucked))
    updatePMove :: PMoveT -> Quake ()
    updatePMove updatedPM
        | (updatedPM^.pmState.pmsPMFlags) .&. Constants.pmfDucked /= 0 =
            pMoveGlobals.pmPM .= (updatedPM & pmMins .~ V3 minsX minsY (-24)
                                            & pmMaxs .~ V3 maxsX maxsY 4
                                            & pmViewHeight .~ -2)
        | otherwise =
            pMoveGlobals.pmPM .= (updatedPM & pmMins .~ V3 minsX minsY (-24)
                                            & pmMaxs .~ V3 maxsX maxsY 32
                                            & pmViewHeight .~ 22)

-- Snaps the origin of the player move to 0.125 grid.
initialSnapPosition :: Quake ()
initialSnapPosition = do
    pm <- use (pMoveGlobals.pmPM)
    checkPosition (pm^.pmState.pmsOrigin) 0 3 0 3 0 3
    
checkPosition :: V3 Int16 -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
checkPosition base z maxZ y maxY x maxX
    | z >= maxZ = Com.dprintf "Bad InitialSnapPosition\n"
    | y >= maxY = checkPosition base (z + 1) maxZ 0 maxY x maxX
    | x >= maxX = checkPosition base z maxZ (y + 1) maxY 0 maxX
    | otherwise = do
        let a = (base^._x) + (offset^.(Math3D.v3Access z))
            b = (base^._y) + (offset^.(Math3D.v3Access y))
            c = (base^._z) + (offset^.(Math3D.v3Access x))
        pMoveGlobals.pmPM.pmState.pmsOrigin .= V3 a b c
        proceedCheckPosition a b c =<< goodPosition
  where
    proceedCheckPosition a b c ok
        | ok = do
            pMoveGlobals.pmPML.pmlOrigin .= fmap ((* 0.125) . fromIntegral) (V3 a b c)
            pMoveGlobals.pmPML.pmlPreviousOrigin .= fmap fromIntegral (V3 a b c)
        | otherwise =
            checkPosition base z maxZ y maxY (x + 1) maxX

catagorizePosition :: Quake ()
catagorizePosition = error "PMove.catagorizePosition" -- TODO

dropTimingCounter :: Quake ()
dropTimingCounter = do
    pm <- use (pMoveGlobals.pmPM)
    when ((pm^.pmState.pmsPMTime) /= 0) $ do
        proceedDropTimingCounter pm

proceedDropTimingCounter :: PMoveT -> Quake ()
proceedDropTimingCounter pm
    | msec' >= pmTime =
        pMoveGlobals.pmPM.pmState %= (\v -> v & pmsPMFlags %~ (.&. (complement (Constants.pmfTimeWaterJump .|. Constants.pmfTimeLand .|. Constants.pmfTimeTeleport)))
                                              & pmsPMTime .~ 0)
    | otherwise =
         pMoveGlobals.pmPM.pmState.pmsPMTime .= fromIntegral (pmTime - msec')
  where
    msec = (pm^.pmCmd.ucMsec) `shiftR` 3
    msec' | msec == 0 = 1
          | otherwise = fromIntegral msec
    pmTime = fromIntegral (pm^.pmState.pmsPMTime) .&. 0xFF :: Int

checkPMFlags :: Quake ()
checkPMFlags = error "PMove.checkPMFlags" -- TODO

snapPosition :: Quake ()
snapPosition = error "PMove.snapPosition" -- TODO

flyMove :: Bool -> Quake ()
flyMove = error "PMove.flyMove" -- TODO

deadMove :: Quake ()
deadMove = do
    pm <- use (pMoveGlobals.pmPM)
    proceedDeadMove (pm^.pmGroundEntity)

proceedDeadMove :: Maybe (Ref' EdictT) -> Quake ()
proceedDeadMove Nothing = return ()
proceedDeadMove _ = do
    pml <- use (pMoveGlobals.pmPML)
    pMoveGlobals.pmPML.pmlVelocity .= calcVelocity (pml^.pmlVelocity)

calcVelocity :: V3 Float -> V3 Float
calcVelocity velocity 
    | forward <= 0 = V3 0 0 0
    | otherwise = fmap (* forward) (normalize velocity)
  where
    forward = norm velocity - 20

goodPosition :: Quake Bool
goodPosition = do
    pm <- use (pMoveGlobals.pmPM)
    checkGoodPosition pm
  where
    checkGoodPosition pm
        | (pm^.pmState.pmsPMType) == Constants.pmSpectator =
            return True
        | otherwise = do
            let origin = fmap ((* 0.125) . fromIntegral) (pm^.pmState.pmsOrigin)
            traceT <- (pm^.pmTrace) origin (pm^.pmMins) (pm^.pmMaxs) origin
            maybe traceError (\v -> return (not (v^.tAllSolid))) traceT
    traceError = do
        Com.fatalError "PMove.goodPosition traceT is Nothing"
        return False

checkSpecialMovement :: Quake ()
checkSpecialMovement = error "PMove.checkSpecialMovement" -- TODO