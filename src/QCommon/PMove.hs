module QCommon.PMove
    ( pMove
    ) where

import           Control.Lens        (use, ix, (^.), (.=), (%=), (+=), (-=), (&), (.~), (%~), (+~), (-~))
import           Control.Monad       (when, unless)
import           Data.Bits           (complement, shiftR, shiftL, (.&.), (.|.))
import           Data.Int            (Int16)
import           Data.Maybe          (isJust, isNothing)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import           Linear              (V3(..), dot, norm, normalize, _x, _y, _z)
import           System.IO.Unsafe    (unsafePerformIO)

import qualified Constants
import           Game.CPlaneT
import           Game.CSurfaceT
import           Game.PMoveStateT
import           Game.PMoveT
import           Game.TraceT
import           Game.UserCmdT
import qualified QCommon.Com         as Com
import           QCommon.PmlT
import           QuakeState
import           Types
import qualified Util.Math3D         as Math3D

offset :: V3 Int16
offset = V3 0 (-1) 1

-- try all single bits first
jitterBits :: UV.Vector Int
jitterBits = UV.fromList [ 0, 4, 1, 2, 3, 5, 6, 7 ]

planes :: MV.IOVector (V3 Float)
planes = unsafePerformIO (V.thaw (V.replicate Constants.maxClipPlanes (V3 0 0 0)))

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
            checkPMFlags =<< use (pMoveGlobals.pmPM)
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
            standUp traceT
        | otherwise =
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
catagorizePosition = do
    point <- getStandingPoint
    checkSolidGround point =<< use (pMoveGlobals.pmPML)
    pMoveGlobals.pmPM.pmWaterLevel .= 0
    pMoveGlobals.pmPM.pmWaterType .= 0
    pm <- use (pMoveGlobals.pmPM)
    pml <- use (pMoveGlobals.pmPML)
    let sample2 = truncate ((pm^.pmViewHeight) - (pm^.pmMins._z)) :: Int
        sample1 = sample2 `div` 2
        V3 a b c = point
        point' = V3 a b ((pml^.pmlOrigin._z) + (pm^.pmMins._z) + 1)
    cont <- (pm^.pmPointContents) point'
    when (cont .&. Constants.maskWater /= 0) $ do
        pMoveGlobals.pmPM.pmWaterType .= cont
        pMoveGlobals.pmPM.pmWaterLevel .= 1
        let c' = ((pml^.pmlOrigin._z) + (pm^.pmMins._z) + fromIntegral sample1)
            point'' = V3 a b c'
        cont' <- (pm^.pmPointContents) point''
        when (cont' .&. Constants.maskWater /= 0) $ do
            pMoveGlobals.pmPM.pmWaterLevel .= 2
            let c'' = ((pml^.pmlOrigin._z) + (pm^.pmMins._z) + fromIntegral sample2)
                point''' = V3 a b c''
            cont'' <- (pm^.pmPointContents) point'''
            when (cont'' .&. Constants.maskWater /= 0) $
                pMoveGlobals.pmPM.pmWaterLevel .= 3
  where
    getStandingPoint = do
        pml <- use (pMoveGlobals.pmPML)
        return (V3 (pml^.pmlOrigin._x) (pml^.pmlOrigin._y) ((pml^.pmlOrigin._z) - 0.25))
    checkSolidGround point pml
        | (pml^.pmlVelocity._z) > 180 = do
            pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.&. (complement Constants.pmfOnGround))
            pMoveGlobals.pmPM.pmGroundEntity .= Nothing
        | otherwise = do
            pm <- use (pMoveGlobals.pmPM)
            traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) point
            pMoveGlobals.pmPML.pmlGroundSurface .= (traceT^.tSurface)
            pMoveGlobals.pmPML.pmlGroundContents .= (traceT^.tContents)
            checkTraceEntity traceT
            pm' <- use (pMoveGlobals.pmPM)
            when ((pm'^.pmNumTouch) < Constants.maxTouch && isJust (traceT^.tEnt)) $ do
                pMoveGlobals.pmPM.pmTouchEnts.ix (pm'^.pmNumTouch) .= (traceT^.tEnt)
                pMoveGlobals.pmPM.pmNumTouch += 1
    checkTraceEntity :: TraceT -> Quake ()
    checkTraceEntity traceT
        | isNothing (traceT^.tEnt) || ((traceT^.tPlane.cpNormal._z) < 0.7 && not (traceT^.tStartSolid)) = do
            pMoveGlobals.pmPM.pmGroundEntity .= Nothing
            pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.&. (complement Constants.pmfOnGround))
        | otherwise = do
            pMoveGlobals.pmPM.pmGroundEntity .= (traceT^.tEnt)
            pm <- use (pMoveGlobals.pmPM)
            -- hitting solid ground will end a waterjump
            when ((pm^.pmState.pmsPMFlags) .&. Constants.pmfTimeWaterJump /= 0) $ do
                pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.&. (complement (Constants.pmfTimeWaterJump .|. Constants.pmfTimeLand .|. Constants.pmfTimeTeleport)))
                pMoveGlobals.pmPM.pmState.pmsPMTime .= 0
            pm' <- use (pMoveGlobals.pmPM)
            when ((pm'^.pmState.pmsPMFlags) .&. Constants.pmfOnGround == 0) $ do
                -- just hit the ground
                pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.|. Constants.pmfOnGround)
                pml <- use (pMoveGlobals.pmPML)
                -- don't do landing time if we were just going down a slope
                when ((pml^.pmlVelocity._z) < -200) $ do
                    pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.|. Constants.pmfTimeLand)
                    -- don't allow another jump for a little while
                    pMoveGlobals.pmPM.pmState.pmsPMTime .= (if (pml^.pmlVelocity._z) < -400 then 25 else 18)

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

checkPMFlags :: PMoveT -> Quake ()
checkPMFlags pm
    | (pm^.pmState.pmsPMFlags) .&. Constants.pmfTimeTeleport /= 0 =
        -- teleport pause stays exactly in place
        return ()
    | (pm^.pmState.pmsPMFlags) .&. Constants.pmfTimeWaterJump /= 0 = do
        -- waterjump has no control, but falls
        pml <- use (pMoveGlobals.pmPML)
        let v = (pml^.pmlVelocity._z) - (fromIntegral (pm^.pmState.pmsGravity)) * (pml^.pmlFrameTime)
        pMoveGlobals.pmPML.pmlVelocity._z .= v
        when (v < 0) $ do
            pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.&. (complement (Constants.pmfTimeWaterJump .|. Constants.pmfTimeLand .|. Constants.pmfTimeTeleport)))
            pMoveGlobals.pmPM.pmState.pmsPMTime .= 0
        stepSlideMove
    | otherwise = do
        checkJump =<< use (pMoveGlobals.pmPM)
        handleFriction
        checkWaterOrAir
  where
    checkWaterOrAir
        | (pm^.pmWaterLevel) >= 2 = waterMove
        | otherwise = do
            let V3 a b c = pm^.pmViewAngles
                a' = (if a > 180 then (a - 360) else a) / 3
                (f, r, u) = Math3D.angleVectors (V3 a' b c) True True True
            pMoveGlobals.pmPML.pmlForward .= f
            pMoveGlobals.pmPML.pmlRight .= r
            pMoveGlobals.pmPML.pmlUp .= u
            airMove

snapPosition :: Quake ()
snapPosition = do
    pml <- use (pMoveGlobals.pmPML)
    -- snap velocity to eights
    let vel = fmap (truncate . (* 8)) (pml^.pmlVelocity)
        sign = fmap (\x -> if x >= 0 then 1 else -1) (pml^.pmlOrigin) :: V3 Int
        origin = fmap (truncate . (* 8)) (pml^.pmlOrigin)
        sign' = let a = if fromIntegral (origin^._x) * 0.125 == (pml^.pmlOrigin._x) then 0 else sign^._x
                    b = if fromIntegral (origin^._y) * 0.125 == (pml^.pmlOrigin._y) then 0 else sign^._y
                    c = if fromIntegral (origin^._z) * 0.125 == (pml^.pmlOrigin._z) then 0 else sign^._z
                in V3 a b c
        base = origin
    pMoveGlobals.pmPM.pmState.pmsVelocity .= vel
    pMoveGlobals.pmPM.pmState.pmsOrigin .= origin
    -- try all combinations
    tryFindingGoodPosition base sign' 0 8
  where
    tryFindingGoodPosition base sign idx maxIdx
        | idx >= maxIdx = do
            -- go back to the last position
            previousOrigin <- use (pMoveGlobals.pmPML.pmlPreviousOrigin)
            pMoveGlobals.pmPM.pmState.pmsOrigin .= fmap truncate previousOrigin
        | otherwise = do
            let bits = jitterBits UV.! idx
                a = if (bits .&. (1 `shiftL` 0)) /= 0 then (base^._x) + fromIntegral (sign^._x) else base^._x
                b = if (bits .&. (1 `shiftL` 1)) /= 0 then (base^._y) + fromIntegral (sign^._y) else base^._y
                c = if (bits .&. (1 `shiftL` 2)) /= 0 then (base^._z) + fromIntegral (sign^._z) else base^._z
            pMoveGlobals.pmPM.pmState.pmsOrigin .= V3 a b c
            ok <- goodPosition
            unless ok $
                tryFindingGoodPosition base sign (idx + 1) maxIdx

flyMove :: Bool -> Quake ()
flyMove doClip = do
    pMoveGlobals.pmPM.pmViewHeight .= 22
    -- friction
    speed <- getSpeed
    setVelocity speed
    -- accelerate
    fmove <- use (pMoveGlobals.pmPM.pmCmd.ucForwardMove)
    smove <- use (pMoveGlobals.pmPM.pmCmd.ucSideMove)
    pMoveGlobals.pmPML.pmlForward %= normalize
    pMoveGlobals.pmPML.pmlRight %= normalize
    pm <- use (pMoveGlobals.pmPM)
    pml <- use (pMoveGlobals.pmPML)
    maxSpeed <- use (pMoveGlobals.pmMaxSpeed)
    let wishVel = let v = fmap (* (fromIntegral fmove)) (pml^.pmlForward) + fmap (* (fromIntegral smove)) (pml^.pmlRight)
                  in v & _z +~ (fromIntegral $ pm^.pmCmd.ucUpMove)
        wishSpeed = norm wishVel
        wishDir = normalize wishVel
        (wishVel', wishSpeed') = if wishSpeed > maxSpeed
                                     then (fmap (* (maxSpeed / wishSpeed)) wishVel, maxSpeed)
                                     else (wishVel, wishSpeed)
        currentSpeed = (pml^.pmlVelocity) `dot` wishDir
        addSpeed = wishSpeed' - currentSpeed
    unless (addSpeed <= 0) $ do
        accelerate <- use (pMoveGlobals.pmAccelerate)
        let accelSpeed = let a = accelerate * (pml^.pmlFrameTime) * wishSpeed'
                         in if a > addSpeed then addSpeed else a
            velocity = (fmap (* accelSpeed) wishDir) + (pml^.pmlVelocity)
        pMoveGlobals.pmPML.pmlVelocity .= velocity
        setOrigin pm pml velocity
  where
    getSpeed = do
      velocity <- use (pMoveGlobals.pmPML.pmlVelocity)
      return (norm velocity)
    setVelocity :: Float -> Quake ()
    setVelocity speed
        | speed < 1 = do
            v3o <- use (globals.gVec3Origin)
            pMoveGlobals.pmPML.pmlVelocity .= v3o
        | otherwise = do
            friction <- fmap (* 1.5) (use (pMoveGlobals.pmFriction)) -- extra friction
            stopSpeed <- use (pMoveGlobals.pmStopSpeed)
            frameTime <- use (pMoveGlobals.pmPML.pmlFrameTime)
            let control = if speed < stopSpeed then stopSpeed else speed
                drop = control * friction * frameTime
                -- scale the velocity
                newSpeed = if speed - drop < 0 then 0 else (speed - drop) / speed
            pMoveGlobals.pmPML.pmlVelocity %= fmap (* newSpeed)
    setOrigin pm pml velocity
        | doClip = do
            let end = (pml^.pmlOrigin) + fmap (* (pml^.pmlFrameTime)) velocity
            traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) end
            pMoveGlobals.pmPML.pmlOrigin .= (traceT^.tEndPos)
        | otherwise =
            pMoveGlobals.pmPML.pmlOrigin += fmap (* (pml^.pmlFrameTime)) velocity

deadMove :: Quake ()
deadMove = do
    pm <- use (pMoveGlobals.pmPM)
    proceedDeadMove (pm^.pmGroundEntity)

proceedDeadMove :: Maybe (Ref EdictT) -> Quake ()
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
            return (not (traceT^.tAllSolid))

checkSpecialMovement :: Quake ()
checkSpecialMovement = do
    pm <- use (pMoveGlobals.pmPM)
    when ((pm^.pmState.pmsPMTime) == 0) $ do
        pMoveGlobals.pmPML.pmlLadder .= False
        -- check for ladder
        pml <- use (pMoveGlobals.pmPML)
        let flatForward = normalize (V3 (pml^.pmlForward._x) (pml^.pmlForward._y) 0)
            spot = (pml^.pmlOrigin) + flatForward
        traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) spot
        when ((traceT^.tFraction) < 1 && ((traceT^.tContents) .&. Constants.contentsLadder /= 0)) $
            pMoveGlobals.pmPML.pmlLadder .= True
        -- check for water jump
        when ((pm^.pmWaterLevel) == 2) $ do
            let V3 a b c = (pml^.pmlOrigin) + fmap (* 30) flatForward
                spot' = V3 a b (c + 4)
            cont <- (pm^.pmPointContents) spot'
            unless (cont .&. Constants.contentsSolid == 0) $ do
                cont' <- (pm^.pmPointContents) (V3 a b (c + 20))
                when (cont' == 0) $ do
                    -- jump out of water
                    pMoveGlobals.pmPML.pmlVelocity .= V3 (50 * (flatForward^._x)) (50 * (flatForward^._y)) 350
                    pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.|. Constants.pmfTimeWaterJump)
                    pMoveGlobals.pmPM.pmState.pmsPMTime .= -1 -- was 255

stepSlideMove :: Quake ()
stepSlideMove = do
    (startO, startV) <- getStartOriginAndVelocity
    stepSlideMove_
    (downO, downV) <- getDownOriginAndVelocity
    let V3 a b c = startO
        up = V3 a b (c + fromIntegral Constants.stepSize)
    traceT <- do
        pm <- use (pMoveGlobals.pmPM)
        (pm^.pmTrace) up (pm^.pmMins) (pm^.pmMaxs) up
    -- can't step up
    unless (traceT^.tAllSolid) $ do
        -- try sliding above
        pMoveGlobals.pmPML.pmlOrigin .= up
        pMoveGlobals.pmPML.pmlVelocity .= startV
        stepSlideMove_
        -- push down the final amount
        pm <- use (pMoveGlobals.pmPM)
        pml <- use (pMoveGlobals.pmPML)
        let V3 a' b' c' = pml^.pmlOrigin
            down = V3 a' b' (c' - fromIntegral Constants.stepSize)
        traceT' <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) down
        unless (traceT'^.tAllSolid) $
            pMoveGlobals.pmPML.pmlOrigin .= (traceT'^.tEndPos)
        up' <- fmap (^.pmlOrigin) (use (pMoveGlobals.pmPML))
        -- decide which one went farther
        let downDist = ((downO^._x) - (startO^._x)) * ((downO^._x) - (startO^._x)) + ((downO^._y) - (startO^._y)) * ((downO^._y) - (startO^._y))
            upDist = ((up'^._x) - (startO^._x)) * ((up'^._x) - (startO^._x)) + ((up'^._y) - (startO^._y)) * ((up'^._y) - (startO^._y))
        if downDist > upDist || (traceT'^.tPlane.cpNormal._z) < Constants.minStepNormal
            then do
                pMoveGlobals.pmPML.pmlOrigin .= downO
                pMoveGlobals.pmPML.pmlVelocity .= downV
            else do
                -- Special case
                -- if we were walking along a plane, then we need to copy the Z over
                pMoveGlobals.pmPML.pmlVelocity._z .= (downV^._z)
  where
    getStartOriginAndVelocity = do
        pml <- use (pMoveGlobals.pmPML)
        return (pml^.pmlOrigin, pml^.pmlVelocity)
    getDownOriginAndVelocity = do
        pml <- use (pMoveGlobals.pmPML)
        return (pml^.pmlOrigin, pml^.pmlVelocity)

stepSlideMove_ :: Quake ()
stepSlideMove_ = do
    pml <- use (pMoveGlobals.pmPML)
    let primalVelocity = pml^.pmlVelocity
        timeLeft = pml^.pmlFrameTime
    done <- slideMove timeLeft primalVelocity 0 0 4
    unless done $ do
        time <- use $ pMoveGlobals.pmPM.pmState.pmsPMTime
        when (time /= 0) $
            pMoveGlobals.pmPML.pmlVelocity .= primalVelocity

slideMove :: Float -> V3 Float -> Int -> Int -> Int -> Quake Bool
slideMove timeLeft primalVelocity numPlanes idx maxIdx
    | idx >= maxIdx = return False
    | otherwise = do
        pm <- use (pMoveGlobals.pmPM)
        pml <- use (pMoveGlobals.pmPML)
        let end = (pml^.pmlOrigin) + fmap (* timeLeft) (pml^.pmlVelocity)
        traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) end
        doSlideMove pm traceT
  where
    doSlideMove :: PMoveT -> TraceT -> Quake Bool
    doSlideMove pm traceT
        | traceT^.tAllSolid = do -- entity is trapped in another solid
            pMoveGlobals.pmPML.pmlVelocity._z .= 0 -- don't build up falling damage
            return True
        | otherwise = do
            numPlanes' <- getNumPlanes traceT
            proceedSlideMove pm traceT numPlanes'
    getNumPlanes :: TraceT -> Quake Int
    getNumPlanes traceT
        | (traceT^.tFraction) > 0 = do -- actually covered some distance
            pMoveGlobals.pmPML.pmlOrigin .= (traceT^.tEndPos)
            return 0
        | otherwise = return numPlanes
    proceedSlideMove pm traceT numPlanes'
        | traceT^.tFraction == 1 = return False
        | otherwise = do
            -- save entity for contact
            when ((pm^.pmNumTouch) < Constants.maxTouch && isJust (traceT^.tEnt)) $ do
                pMoveGlobals.pmPM.pmTouchEnts.ix (pm^.pmNumTouch) .= (traceT^.tEnt)
                pMoveGlobals.pmPM.pmNumTouch += 1
            slideAlongPlane primalVelocity numPlanes' (timeLeft - timeLeft * (traceT^.tFraction))

slideAlongPlane :: V3 Float -> Int -> Float -> Quake Bool
slideAlongPlane primalVelocity numPlanes timeLeft
    | numPlanes >= Constants.maxClipPlanes = do
        -- this shouldn't really happen
        v3o <- use (globals.gVec3Origin)
        pMoveGlobals.pmPML.pmlVelocity .= v3o
        return False
    | otherwise = do
        undefined -- TODO

checkJump :: PMoveT -> Quake ()
checkJump pm
    | (pm^.pmState.pmsPMFlags) .&. Constants.pmfTimeLand /= 0 =
        -- hasn't been long enough since landing to jump again
        return ()
    | (pm^.pmCmd.ucUpMove) < 10 = -- not holding jump
        pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.&. (complement Constants.pmfJumpHeld))
    | (pm^.pmState.pmsPMFlags) .&. Constants.pmfJumpHeld /= 0 =
        -- must wait for jump to be released
        return ()
    | (pm^.pmState.pmsPMType) == Constants.pmDead =
        return ()
    | (pm^.pmWaterLevel) >= 2 = do
        -- swimming, not jumping
        pMoveGlobals.pmPM.pmGroundEntity .= Nothing
        pml <- use (pMoveGlobals.pmPML)
        unless ((pml^.pmlVelocity._z) <= -300) $ do
            let v | (pm^.pmWaterType) == Constants.contentsWater = 100
                  | (pm^.pmWaterType) == Constants.contentsSlime = 80
                  | otherwise = 50
            pMoveGlobals.pmPML.pmlVelocity._z .= v
    | isNothing (pm^.pmGroundEntity) =
        -- in air, so no effect
        return ()
    | otherwise = do
        pml <- use (pMoveGlobals.pmPML)
        let v = max ((pml^.pmlVelocity._z) + 270) 270
        pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.|. Constants.pmfJumpHeld)
        pMoveGlobals.pmPM.pmGroundEntity .= Nothing
        pMoveGlobals.pmPML.pmlVelocity._z .= v

-- Handles both ground friction and water friction.
handleFriction :: Quake ()
handleFriction = do
    pml <- use (pMoveGlobals.pmPML)
    doHandleFriction pml (norm (pml^.pmlVelocity))
  where
    doHandleFriction :: PmlT -> Float -> Quake ()
    doHandleFriction pml speed
        | speed < 1 =
            pMoveGlobals.pmPML.pmlVelocity .= V3 0 0 (pml^.pmlVelocity._z)
        | otherwise = do
            pm <- use (pMoveGlobals.pmPM)
            f <- use (pMoveGlobals.pmFriction)
            stopSpeed <- use (pMoveGlobals.pmStopSpeed)
            let (control, drop) = applyGroundFriction pm pml (pml^.pmlGroundSurface) f speed stopSpeed
            -- apply water friction
            waterFriction <- use (pMoveGlobals.pmWaterFriction)
            let drop' | (pm^.pmWaterLevel) /= 0 && not (pml^.pmlLadder) =
                          drop + speed * waterFriction * (fromIntegral $ pm^.pmWaterLevel) * (pml^.pmlFrameTime)
                      | otherwise = drop
            -- scale the velocity
            let newSpeed = if speed - drop' < 0 then 0 else (speed - drop') / speed
            pMoveGlobals.pmPML.pmlVelocity %= (fmap (* newSpeed))
    applyGroundFriction _ _ Nothing _ _ _ = (0, 0)
    applyGroundFriction pm pml (Just groundSurface) f speed stopSpeed
        | isJust (pm^.pmGroundEntity) && ((groundSurface^.csFlags) .&. Constants.surfSlick) == 0 =
            let ctrl = if speed < stopSpeed then stopSpeed else speed
                drop = ctrl * f * (pml^.pmlFrameTime)
            in (ctrl, drop)
        | otherwise = (0, 0)

waterMove :: Quake ()
waterMove = do
    pm <- use (pMoveGlobals.pmPM)
    pml <- use (pMoveGlobals.pmPML)
    waterSpeed <- use (pMoveGlobals.pmWaterSpeed)
    -- user intentions
    let wishVel = let vel = fmap (* (fromIntegral $ pm^.pmCmd.ucForwardMove)) (pml^.pmlForward) + fmap (* (fromIntegral $ pm^.pmCmd.ucSideMove)) (pml^.pmlRight)
                  in if (pm^.pmCmd.ucForwardMove) == 0 && (pm^.pmCmd.ucSideMove) == 0 && (pm^.pmCmd.ucUpMove) == 0
                         then vel & _z -~ 60 -- drift towards bottom
                         else vel & _z +~ fromIntegral (pm^.pmCmd.ucUpMove)
        wishVel' = addCurrents pm pml wishVel waterSpeed
        wishDir = normalize wishVel'
        wishSpeed = norm wishVel'
    maxSpeed <- use (pMoveGlobals.pmMaxSpeed)
    let (wishVel'', wishSpeed') = if wishSpeed > maxSpeed
                                      then (fmap (* (maxSpeed / wishSpeed)) wishVel', maxSpeed * 0.5)
                                      else (wishVel', wishSpeed * 0.5)
    accel <- use (pMoveGlobals.pmWaterAccelerate)
    accelerate wishDir wishSpeed' accel
    stepSlideMove

airMove :: Quake ()
airMove = do
    pm <- use (pMoveGlobals.pmPM)
    pml <- use (pMoveGlobals.pmPML)
    waterSpeed <- use (pMoveGlobals.pmWaterSpeed)
    let fmove = fromIntegral (pm^.pmCmd.ucForwardMove)
        smove = fromIntegral (pm^.pmCmd.ucSideMove)
        wishVel = V3 ((pml^.pmlForward._x) * fmove + (pml^.pmlRight._x) * smove) ((pml^.pmlForward._y) * fmove + (pml^.pmlRight._y) * smove) 0
        wishVel' = addCurrents pm pml wishVel waterSpeed
        wishDir = normalize wishVel'
        wishSpeed = norm wishVel'
    -- clamp to server defined max speed
    maxSpeed <- if (pm^.pmState.pmsPMFlags) .&. Constants.pmfDucked /= 0
                    then use (pMoveGlobals.pmDuckSpeed)
                    else use (pMoveGlobals.pmMaxSpeed)
    let (wishVel'', wishSpeed') = if wishSpeed > maxSpeed
                                      then (fmap (* (maxSpeed / wishSpeed)) wishVel', maxSpeed)
                                      else (wishVel', wishSpeed)
    doAirMove pm pml wishDir wishVel'' wishSpeed'
  where
    doAirMove pm pml wishDir wishVel wishSpeed
        | pml^.pmlLadder = do
            accel <- use (pMoveGlobals.pmAccelerate)
            accelerate wishDir wishSpeed accel
            when ((wishVel^._z) == 0) $
                if (pml^.pmlVelocity._z) > 0
                    then do
                        pMoveGlobals.pmPML.pmlVelocity._z -= (fromIntegral (pm^.pmState.pmsGravity)) * (pml^.pmlFrameTime)
                        velocityZ <- use (pMoveGlobals.pmPML.pmlVelocity._z)
                        when (velocityZ < 0) $
                            pMoveGlobals.pmPML.pmlVelocity._z .= 0
                    else do
                        pMoveGlobals.pmPML.pmlVelocity._z += (fromIntegral (pm^.pmState.pmsGravity)) * (pml^.pmlFrameTime)
                        velocityZ <- use (pMoveGlobals.pmPML.pmlVelocity._z)
                        when (velocityZ > 0) $
                            pMoveGlobals.pmPML.pmlVelocity._z .= 0
            stepSlideMove
        | isJust (pm^.pmGroundEntity) = do -- walking on ground
            pMoveGlobals.pmPML.pmlVelocity._z .= 0 -- !!! this is before the accel
            accel <- use (pMoveGlobals.pmAccelerate)
            accelerate wishDir wishSpeed accel
            if (pm^.pmState.pmsGravity) > 0
                then pMoveGlobals.pmPML.pmlVelocity._z .= 0 -- !!! this is before the accel
                else pMoveGlobals.pmPML.pmlVelocity._z -= (fromIntegral (pm^.pmState.pmsGravity)) * (pml^.pmlFrameTime)
            velocity <- use (pMoveGlobals.pmPML.pmlVelocity)
            unless ((velocity^._x) == 0 && (velocity^._y) == 0) $
                stepSlideMove
        | otherwise = do -- not on ground, so little effect on velocity
            airAccel <- use (pMoveGlobals.pmAirAccelerate)
            if airAccel /= 0
                then do
                    accel <- use (pMoveGlobals.pmAccelerate)
                    airAccelerate wishDir wishSpeed accel
                else
                    accelerate wishDir wishSpeed 1
            -- add gravity
            pMoveGlobals.pmPML.pmlVelocity._z -= (fromIntegral $ pm^.pmState.pmsGravity) * (pml^.pmlFrameTime)
            stepSlideMove

addCurrents :: PMoveT -> PmlT -> V3 Float -> Float -> V3 Float
addCurrents pm pml (V3 a b c) waterSpeed =
    -- account for ladders
    let wishVel = if (pml^.pmlLadder) && abs(pml^.pmlVelocity._z) <= 200
                    then let c' | (pm^.pmViewAngles.(Math3D.v3Access Constants.pitch)) <= -15 && (pm^.pmCmd.ucForwardMove) > 0 = 200
                                | (pm^.pmViewAngles.(Math3D.v3Access Constants.pitch)) >= 15 && (pm^.pmCmd.ucForwardMove) > 0 = -200
                                | (pm^.pmCmd.ucUpMove) > 0 = 200
                                | (pm^.pmCmd.ucUpMove) < 0 = -200
                                | otherwise = 0
                             -- limit horizontal speed when on a ladder
                             a' | a < -25 = -25
                                | a > 25 = 25
                                | otherwise = a
                             b' | b < -25 = -25
                                | b > 25 = 25
                                | otherwise = b
                         in V3 a' b' c'
                    else V3 a b c
        -- add water currents
        v = if (pm^.pmWaterType) .&. Constants.maskCurrent /= 0
              then let va = if (pm^.pmWaterType) .&. Constants.contentsCurrent0 /= 0 then 1 else 0
                       vb = if (pm^.pmWaterType) .&. Constants.contentsCurrent90 /= 0 then 1 else 0
                       vc = if (pm^.pmWaterType) .&. Constants.contentsCurrent180 /= 0 then -1 else 0
                       vd = if (pm^.pmWaterType) .&. Constants.contentsCurrent270 /= 0 then -1 else 0
                       ve = if (pm^.pmWaterType) .&. Constants.contentsCurrentUp /= 0 then 1 else 0
                       vf = if (pm^.pmWaterType) .&. Constants.contentsCurrentDown /= 0 then -1 else 0
                   in V3 (va + vc) (vb + vd) (ve + vf)
              else V3 0 0 0
        s = if (pm^.pmWaterLevel) == 1 && isJust (pm^.pmGroundEntity) then waterSpeed / 2 else waterSpeed
        wishVel' = wishVel + fmap (* s) v
        -- add conveyor belt velocities
        v' = if isJust (pm^.pmGroundEntity)
               then let va = if (pml^.pmlGroundContents) .&. Constants.contentsCurrent0 /= 0 then 1 else 0
                        vb = if (pml^.pmlGroundContents) .&. Constants.contentsCurrent90 /= 0 then 1 else 0
                        vc = if (pml^.pmlGroundContents) .&. Constants.contentsCurrent180 /= 0 then -1 else 0
                        vd = if (pml^.pmlGroundContents) .&. Constants.contentsCurrent270 /= 0 then -1 else 0
                        ve = if (pml^.pmlGroundContents) .&. Constants.contentsCurrentUp /= 0 then 1 else 0
                        vf = if (pml^.pmlGroundContents) .&. Constants.contentsCurrentDown /= 0 then -1 else 0
                    in V3 (va + vc) (vb + vd) (ve + vf)
               else V3 0 0 0
        wishVel'' = wishVel' + fmap (* 100) v'
    in wishVel''

-- Handles user intended acceleration
accelerate :: V3 Float -> Float -> Float -> Quake ()
accelerate wishDir wishSpeed accel = do
    pml <- use (pMoveGlobals.pmPML)
    let currentSpeed = (pml^.pmlVelocity) `dot` wishDir
        addSpeed = wishSpeed - currentSpeed
    unless (addSpeed <= 0) $ do
        let accelSpeed = min (accel * (pml^.pmlFrameTime) * wishSpeed) addSpeed
        pMoveGlobals.pmPML.pmlVelocity += (fmap (* accelSpeed) wishDir)

airAccelerate :: V3 Float -> Float -> Float -> Quake ()
airAccelerate wishDir wishSpeed accel = do
    pml <- use (pMoveGlobals.pmPML)
    let wishSpd = if wishSpeed > 30 then 30 else wishSpeed
        currentSpeed = (pml^.pmlVelocity) `dot` wishDir
        addSpeed = wishSpd - currentSpeed
    unless (addSpeed <= 0) $ do
        let accelSpeed = min (accel * wishSpeed * (pml^.pmlFrameTime)) addSpeed
        pMoveGlobals.pmPML.pmlVelocity += (fmap (* accelSpeed) wishDir)
