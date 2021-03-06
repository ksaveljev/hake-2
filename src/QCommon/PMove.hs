{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.PMove where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), (+=), (-=), (&), (+~), (-~))
import Control.Monad (when, unless)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Int (Int16)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _x, _y, _z, normalize, norm, dot, cross)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Game.TraceT
import Game.PMoveT
import QCommon.PmlT
import Game.CSurfaceT
import Game.PMoveStateT
import Game.UserCmdT
import Types
import QuakeState
import qualified Constants
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified Util.Math3D as Math3D

-- try all single bits first
jitterBits :: UV.Vector Int
jitterBits = UV.fromList [ 0, 4, 1, 2, 3, 5, 6, 7 ]

offset :: V3 Int16
offset = V3 0 (-1) 1

-- Can be called by either the server or the client
pMove :: PMoveT -> Quake PMoveT
pMove pmove = do
    -- clear results
    pMoveGlobals.pmPM .= pmove { _pmNumTouch     = 0
                               , _pmViewAngles   = V3 0 0 0
                               , _pmViewHeight   = 0
                               , _pmGroundEntity = Nothing
                               , _pmWaterType    = 0
                               , _pmWaterLevel   = 0
                               }

    pml <- use $ pMoveGlobals.pmPML
    pMoveGlobals.pmPML .= pml { _pmlGroundSurface  = Nothing
                              , _pmlGroundContents = 0
                              -- convert origin and velocity to float values
                              , _pmlOrigin         = fmap ((* 0.125) . fromIntegral) (pmove^.pmState.pmsOrigin)
                              , _pmlVelocity       = fmap ((* 0.125) . fromIntegral) (pmove^.pmState.pmsVelocity)
                              -- save old org in case we get stuck
                              , _pmlPreviousOrigin = fmap fromIntegral (pmove^.pmState.pmsOrigin)
                              , _pmlFrameTime      = fromIntegral (fromIntegral (pmove^.pmCmd.ucMsec) .&. 0xFF :: Int) * 0.001
                              }

    clampAngles

    if (pmove^.pmState.pmsPMType) == Constants.pmSpectator
      then do
        flyMove False
        snapPosition
        use (pMoveGlobals.pmPM)
      else do
        when ((pmove^.pmState.pmsPMType) >= Constants.pmDead) $
          zoom (pMoveGlobals.pmPM.pmCmd) $ do
            ucForwardMove .= 0
            ucSideMove .= 0
            ucUpMove .= 0

        if (pmove^.pmState.pmsPMType) == Constants.pmFreeze
          then do -- no movement at all
            use (pMoveGlobals.pmPM)
          else do
            -- set mins, maxs, and viewheight
            checkDuck

            when (pmove^.pmSnapInitial) $
              initialSnapPosition

            -- set groundentity, watertype, and waterlevel
            catagorizePosition

            when ((pmove^.pmState.pmsPMType) == Constants.pmDead) $
              deadMove

            checkSpecialMovement

            dropTimingCounter

            checkPMFlags

            -- set groundentity, watertype, and waterlevel for final spot
            catagorizePosition
            snapPosition

            use (pMoveGlobals.pmPM)

  where dropTimingCounter :: Quake ()
        dropTimingCounter = do
          pm <- use $ pMoveGlobals.pmPM

          when ((pm^.pmState.pmsPMTime) /= 0) $ do
            let msec = (pm^.pmCmd.ucMsec) `shiftR` 3
                msec' = if msec == 0 then 1 else fromIntegral msec
                pmTime = fromIntegral (pm^.pmState.pmsPMTime) .&. 0xFF :: Int

            if msec' >= pmTime
              then
                zoom (pMoveGlobals.pmPM.pmState) $ do
                  pmsPMFlags %= (.&. (complement (pmfTimeWaterJump .|. pmfTimeLand .|. pmfTimeTeleport)))
                  pmsPMTime .= 0
              else
                pMoveGlobals.pmPM.pmState.pmsPMTime .= fromIntegral (pmTime - msec')

        checkPMFlags :: Quake ()
        checkPMFlags = do
          pm <- use $ pMoveGlobals.pmPM

          if | (pm^.pmState.pmsPMFlags) .&. pmfTimeTeleport /= 0 ->
                 -- teleport pause stays exactly in place
                 return ()

             | (pm^.pmState.pmsPMFlags) .&. pmfTimeWaterJump /= 0 -> do
                 -- waterjump has no control, but falls
                 pml <- use $ pMoveGlobals.pmPML
                 let v = (pml^.pmlVelocity._z) - (fromIntegral $ pm^.pmState.pmsGravity) * (pml^.pmlFrameTime)
                 pMoveGlobals.pmPML.pmlVelocity._z .= v

                 when (v < 0) $
                   zoom (pMoveGlobals.pmPM.pmState) $ do
                     pmsPMFlags %= (.&. (complement (pmfTimeWaterJump .|. pmfTimeLand .|. pmfTimeTeleport)))
                     pmsPMTime .= 0

                 stepSlideMove

             | otherwise -> do
                 checkJump

                 friction

                 if (pm^.pmWaterLevel) >= 2
                   then
                     waterMove
                   else do
                     -- TODO: think how to use Constants.pitch instead of
                     -- using _x directly
                     let V3 a b c = pm^.pmViewAngles
                         a' = (if a > 180 then (a - 360) else a) / 3
                         (Just f, Just r, Just u) = Math3D.angleVectors (V3 a' b c) True True True

                     zoom (pMoveGlobals.pmPML) $ do
                       pmlForward .= f
                       pmlRight .= r
                       pmlUp .= u

                     airMove

clampAngles :: Quake ()
clampAngles = do
    pm <- use $ pMoveGlobals.pmPM

    -- io $ print "CLAMP ANGLES"
    -- io $ print (pm^.pmCmd.ucAngles)
    -- io $ print (pm^.pmState.pmsDeltaAngles)

    let pm' = if (pm^.pmState.pmsPMFlags) .&. pmfTimeTeleport /= 0
                then
                  -- TODO: think how to update it using Constants.yaw,
                  -- Constants.pitch and Constants.roll
                  pm { _pmViewAngles = V3 0 (fromIntegral $ (pm^.pmCmd.ucAngles._y) + (pm^.pmState.pmsDeltaAngles._y)) 0 }
                else
                  -- circularly clamp the angles with deltas
                  let V3 a b c = fmap Math3D.shortToAngle (fmap fromIntegral $ (pm^.pmCmd.ucAngles) + (pm^.pmState.pmsDeltaAngles))
                      a' = if | a > 89 && a < 180 -> 89
                              | a < 271 && a >= 180 -> 271
                              | otherwise -> a
                  in pm { _pmViewAngles = V3 a' b c }

    pMoveGlobals.pmPM .= pm'

    let (Just forward, Just right, Just up) = Math3D.angleVectors (pm'^.pmViewAngles) True True True
    
    zoom (pMoveGlobals.pmPML) $ do
      pmlForward .= forward
      pmlRight .= right
      pmlUp .= up

flyMove :: Bool -> Quake ()
flyMove doClip = do
    pMoveGlobals.pmPM.pmViewHeight .= 22

    -- friction
    speed <- use (pMoveGlobals.pmPML.pmlVelocity) >>= \velocity -> return (norm velocity)

    if speed < 1
      then do
        v3o <- use $ globals.gVec3Origin
        pMoveGlobals.pmPML.pmlVelocity .= v3o

      else do
        friction <- use (pMoveGlobals.pmFriction) >>= \f -> return (f * 1.5) -- extra friction
        stopSpeed <- use $ pMoveGlobals.pmStopSpeed
        frameTime <- use $ pMoveGlobals.pmPML.pmlFrameTime

        let control = if speed < stopSpeed then stopSpeed else speed
            drop = control * friction * frameTime
            -- scale the velocity
            newSpeed = if speed - drop < 0 then 0 else (speed - drop) / speed

        pMoveGlobals.pmPML.pmlVelocity %= fmap (* newSpeed)

    -- accelerate
    fmove <- use $ pMoveGlobals.pmPM.pmCmd.ucForwardMove
    smove <- use $ pMoveGlobals.pmPM.pmCmd.ucSideMove

    zoom (pMoveGlobals.pmPML) $ do
      pmlForward %= normalize
      pmlRight %= normalize

    pm <- use $ pMoveGlobals.pmPM
    pml <- use $ pMoveGlobals.pmPML
    
    let wishVel = let v = fmap (* (fromIntegral fmove)) (pml^.pmlForward) + fmap (* (fromIntegral smove)) (pml^.pmlRight)
                  in v & _z +~ (fromIntegral $ pm^.pmCmd.ucUpMove)
        wishSpeed = norm wishVel
        wishDir = normalize wishVel

    maxSpeed <- use $ pMoveGlobals.pmMaxSpeed

    -- clamp to server defined max speed
    let (wishVel', wishSpeed') = if wishSpeed > maxSpeed
                                   then (fmap (* (maxSpeed / wishSpeed)) wishVel, maxSpeed)
                                   else (wishVel, wishSpeed)
        currentSpeed = (pml^.pmlVelocity) `dot` wishDir
        addSpeed = wishSpeed' - currentSpeed

    unless (addSpeed <= 0) $ do
      accelerate <- use $ pMoveGlobals.pmAccelerate

      let accelSpeed = let a = accelerate * (pml^.pmlFrameTime) * wishSpeed'
                       in if a > addSpeed then addSpeed else a
          velocity = (fmap (* accelSpeed) wishDir) + (pml^.pmlVelocity)

      pMoveGlobals.pmPML.pmlVelocity .= velocity

      if doClip
        then do
          let end = (pml^.pmlOrigin) + fmap (* (pml^.pmlFrameTime)) velocity

          Just traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) end
          pMoveGlobals.pmPML.pmlOrigin .= (traceT^.tEndPos)

        else do
          -- move
          pMoveGlobals.pmPML.pmlOrigin += fmap (* (pml^.pmlFrameTime)) velocity

{-
- On exit, the origin will have a value that is pre-quantized to the 0.125
- precision of the network channel and in a valid position.
-}
snapPosition :: Quake ()
snapPosition = do
    pm <- use $ pMoveGlobals.pmPM
    pml <- use $ pMoveGlobals.pmPML

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

  where tryFindingGoodPosition :: V3 Int16 -> V3 Int -> Int -> Int -> Quake ()
        tryFindingGoodPosition base sign idx maxIdx
          | idx >= maxIdx = do
              -- go back to the last position
              previousOrigin <- use $ pMoveGlobals.pmPML.pmlPreviousOrigin
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

goodPosition :: Quake Bool
goodPosition = do
    pm <- use $ pMoveGlobals.pmPM

    if (pm^.pmState.pmsPMType) == Constants.pmSpectator
      then
        return True
      else do
        let origin = fmap ((* 0.125) . fromIntegral) (pm^.pmState.pmsOrigin)

        Just traceT <- (pm^.pmTrace) origin (pm^.pmMins) (pm^.pmMaxs) origin

        return $ not (traceT^.tAllSolid)

-- Sets mins, maxs, and pm.viewheight.
checkDuck :: Quake ()
checkDuck = do
    let minsX = -16
        minsY = -16
        maxsX = 16
        maxsY = 16

    pm <- use $ pMoveGlobals.pmPM

    if (pm^.pmState.pmsPMType) == Constants.pmGib
      then do
        pMoveGlobals.pmPM .= pm { _pmMins       = V3 minsX minsY 0
                                , _pmMaxs       = V3 maxsX maxsY 16
                                , _pmViewHeight = 8
                                }
      else do
        pm' <- if | (pm^.pmState.pmsPMType) == Constants.pmDead ->
                      return pm { _pmState = (pm^.pmState) { _pmsPMFlags = (pm^.pmState.pmsPMFlags) .|. pmfDucked} }

                  | (pm^.pmCmd.ucUpMove) < 0 && ((pm^.pmState.pmsPMFlags) .&. pmfOnGround /= 0) -> -- duck
                      return pm { _pmState = (pm^.pmState) { _pmsPMFlags = (pm^.pmState.pmsPMFlags) .|. pmfDucked} }

                  | otherwise -> do -- stand up if possible
                      if (pm^.pmState.pmsPMFlags) .&. pmfDucked /= 0
                        then do
                          pml <- use $ pMoveGlobals.pmPML
                          Just traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (V3 (-16) (-16) (-24)) (V3 16 16 32) (pml^.pmlOrigin)
                          if traceT^.tAllSolid
                            then return pm
                            else return pm { _pmState = (pm^.pmState) { _pmsPMFlags = (pm^.pmState.pmsPMFlags) .&. (complement pmfDucked)} }
                        else
                          return pm

        if (pm'^.pmState.pmsPMFlags) .&. pmfDucked /= 0
          then do
            pMoveGlobals.pmPM .= pm' { _pmMins       = V3 minsX minsY (-24)
                                     , _pmMaxs       = V3 maxsX maxsY 4
                                     , _pmViewHeight = -2
                                     }
          else do
            pMoveGlobals.pmPM .= pm' { _pmMins       = V3 minsX minsY (-24)
                                     , _pmMaxs       = V3 maxsX maxsY 32
                                     , _pmViewHeight = 22
                                     }

-- Snaps the origin of the player move to 0.125 grid.
initialSnapPosition :: Quake ()
initialSnapPosition = do
    pm <- use $ pMoveGlobals.pmPM
    let base = pm^.pmState.pmsOrigin
    checkPosition base 0 3 0 3 0 3

  where checkPosition :: V3 Int16 -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
        checkPosition base z maxZ y maxY x maxX
          | z >= maxZ = Com.dprintf "Bad InitialSnapPosition\n"
          | y >= maxY = checkPosition base (z + 1) maxZ 0 maxY x maxX
          | x >= maxX = checkPosition base z maxZ (y + 1) maxY 0 maxX
          | otherwise = do
              let a = (base^._x) + (offset^.(Math3D.v3Access z))
                  b = (base^._y) + (offset^.(Math3D.v3Access y))
                  c = (base^._z) + (offset^.(Math3D.v3Access x))
              pMoveGlobals.pmPM.pmState.pmsOrigin .= V3 a b c
              ok <- goodPosition
              if ok
                then do
                  pMoveGlobals.pmPML.pmlOrigin .= fmap ((* 0.125) . fromIntegral) (V3 a b c)
                  pMoveGlobals.pmPML.pmlPreviousOrigin .= fmap fromIntegral (V3 a b c)
                else
                  checkPosition base z maxZ y maxY (x + 1) maxX

catagorizePosition :: Quake ()
catagorizePosition = do
    -- if the player hull point one unit down is solid, the player
    -- is on ground

    -- see if standing on something solid
    pml <- use $ pMoveGlobals.pmPML
    let point = V3 (pml^.pmlOrigin._x) (pml^.pmlOrigin._y) ((pml^.pmlOrigin._z) - 0.25)

    if (pml^.pmlVelocity._z) > 180 -- !! ZOID changed from 100 to 180 (ramp accel)
      then do
        zoom (pMoveGlobals.pmPM) $ do
          pmState.pmsPMFlags %= (.&. (complement pmfOnGround))
          pmGroundEntity .= Nothing
      else do
        pm <- use $ pMoveGlobals.pmPM
        Just traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) point

        pMoveGlobals.pmPML.pmlGroundSurface .= (traceT^.tSurface)
        pMoveGlobals.pmPML.pmlGroundContents .= (traceT^.tContents)

        if isNothing (traceT^.tEnt) || ((traceT^.tPlane.cpNormal._z) < 0.7 && not (traceT^.tStartSolid))
          then do
            zoom (pMoveGlobals.pmPM) $ do
              pmGroundEntity .= Nothing
              pmState.pmsPMFlags %= (.&. (complement pmfOnGround))
          else do
            pMoveGlobals.pmPM.pmGroundEntity .= (traceT^.tEnt)

            -- hitting solid ground will end a waterjump
            when ((pm^.pmState.pmsPMFlags) .&. pmfTimeWaterJump /= 0) $
              zoom (pMoveGlobals.pmPM.pmState) $ do
                pmsPMFlags %= (.&. (complement (pmfTimeWaterJump .|. pmfTimeLand .|. pmfTimeTeleport)))
                pmsPMTime .= 0

            pm' <- use $ pMoveGlobals.pmPM

            when ((pm'^.pmState.pmsPMFlags) .&. pmfOnGround == 0) $ do
              -- just hit the ground
              pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.|. pmfOnGround)

              -- don't do landing time if we were just going down a slope
              when ((pml^.pmlVelocity._z) < -200) $ do
                pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.|. pmfTimeLand)
                -- don't allow another jump for a little while
                if (pml^.pmlVelocity._z) < -400
                  then pMoveGlobals.pmPM.pmState.pmsPMTime .= 25
                  else pMoveGlobals.pmPM.pmState.pmsPMTime .= 18

        pm' <- use $ pMoveGlobals.pmPM

        when ((pm'^.pmNumTouch) < Constants.maxTouch && isJust (traceT^.tEnt)) $
          zoom (pMoveGlobals.pmPM) $ do
            pmTouchEnts.ix (pm'^.pmNumTouch) .= fromJust (traceT^.tEnt)
            pmNumTouch += 1

    -- get waterlevel, accounting for ducking
    zoom (pMoveGlobals.pmPM) $ do
      pmWaterLevel .= 0
      pmWaterType .= 0

    pm <- use $ pMoveGlobals.pmPM
    pml' <- use $ pMoveGlobals.pmPML

    let sample2 = truncate ((pm^.pmViewHeight) - (pm^.pmMins._z)) :: Int
        sample1 = sample2 `div` 2
        V3 a b c = point
        point' = V3 a b ((pml'^.pmlOrigin._z) + (pm^.pmMins._z) + 1)

    cont <- (pm^.pmPointContents) point'

    when (cont .&. Constants.maskWater /= 0) $ do
      zoom (pMoveGlobals.pmPM) $ do
        pmWaterType .= cont
        pmWaterLevel .= 1

      let c' = ((pml'^.pmlOrigin._z) + (pm^.pmMins._z) + fromIntegral sample1)
          point'' = V3 a b c'

      cont' <- (pm^.pmPointContents) point''

      when (cont' .&. Constants.maskWater /= 0) $ do
        pMoveGlobals.pmPM.pmWaterLevel .= 2

        let c'' = ((pml'^.pmlOrigin._z) + (pm^.pmMins._z) + fromIntegral sample2)
            point''' = V3 a b c''

        cont'' <- (pm^.pmPointContents) point'''
            
        when (cont'' .&. Constants.maskWater /= 0) $
          pMoveGlobals.pmPM.pmWaterLevel .= 3

-- Dead bodies have extra friction.
deadMove :: Quake ()
deadMove = do
    pm <- use $ pMoveGlobals.pmPM

    case pm^.pmGroundEntity of
      Nothing ->
        return ()

      Just _ -> do
        -- extra friction
        pml <- use $ pMoveGlobals.pmPML

        let forward = norm (pml^.pmlVelocity) - 20

        pMoveGlobals.pmPML.pmlVelocity .= if forward <= 0
                                            then V3 0 0 0
                                            else fmap (* forward) (normalize (pml^.pmlVelocity))

checkSpecialMovement :: Quake ()
checkSpecialMovement = do
    pm <- use $ pMoveGlobals.pmPM

    when ((pm^.pmState.pmsPMTime) == 0) $ do
      pMoveGlobals.pmPML.pmlLadder .= False

      -- check for ladder
      pml <- use $ pMoveGlobals.pmPML
      let flatForward = normalize (V3 (pml^.pmlForward._x) (pml^.pmlForward._y) 0)
          spot = (pml^.pmlOrigin) + flatForward

      Just traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) spot

      when ((traceT^.tFraction) < 1 && ((traceT^.tContents) .&. Constants.contentsLadder /= 0)) $
        pMoveGlobals.pmPML.pmlLadder .= True

      -- check for water jump
      when ((pm^.pmWaterLevel) == 2) $ do
        let V3 a b c = (pml^.pmlOrigin) + fmap (* 30) flatForward
            spot' = V3 a b (c + 4)

        cont <- (pm^.pmPointContents) (V3 a b (c + 4))

        unless (cont .&. Constants.contentsSolid == 0) $ do
          cont' <- (pm^.pmPointContents) (V3 a b (c + 20))

          when (cont' == 0) $ do
            -- jump out of water
            zoom pMoveGlobals $ do
              pmPML.pmlVelocity .= V3 (50 * (flatForward^._x)) (50 * (flatForward^._y)) 350
              pmPM.pmState.pmsPMFlags %= (.|. pmfTimeWaterJump)
              pmPM.pmState.pmsPMTime .= -1 -- was 255

{-
- Each intersection will try to step over the obstruction instead of 
- sliding along it.
- 
- Returns a new origin, velocity, and contact entity.
- Does not modify any world state?
-}
stepSlideMove :: Quake ()
stepSlideMove = do
    (startO, startV) <- do
      pml <- use $ pMoveGlobals.pmPML
      return (pml^.pmlOrigin, pml^.pmlVelocity)

    stepSlideMove_

    (downO, downV) <- do
      pml <- use $ pMoveGlobals.pmPML
      return (pml^.pmlOrigin, pml^.pmlVelocity)

    let V3 a b c = startO
        up = V3 a b (c + fromIntegral Constants.stepSize)

    Just traceT <- use (pMoveGlobals.pmPM) >>= \pm -> (pm^.pmTrace) up (pm^.pmMins) (pm^.pmMaxs) up

            -- can't step up
    unless (traceT^.tAllSolid) $ do
      -- try sliding above
      pMoveGlobals.pmPML.pmlOrigin .= up
      pMoveGlobals.pmPML.pmlVelocity .= startV

      stepSlideMove_

      -- push down the final amount
      pm <- use $ pMoveGlobals.pmPM
      pml <- use $ pMoveGlobals.pmPML
      let V3 a' b' c' = pml^.pmlOrigin
          down = V3 a' b' (c' - fromIntegral Constants.stepSize)

      Just traceT' <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) down
      unless (traceT'^.tAllSolid) $
        pMoveGlobals.pmPML.pmlOrigin .= (traceT'^.tEndPos)

      up <- use (pMoveGlobals.pmPML) >>= \pml' -> return (pml'^.pmlOrigin)

      -- decide which one went farther
      let downDist = ((downO^._x) - (startO^._x)) * ((downO^._x) - (startO^._x)) + ((downO^._y) - (startO^._y)) * ((downO^._y) - (startO^._y))
          upDist = ((up^._x) - (startO^._x)) * ((up^._x) - (startO^._x)) + ((up^._y) - (startO^._y)) * ((up^._y) - (startO^._y))

      if downDist > upDist || (traceT'^.tPlane.cpNormal._z) < Constants.minStepNormal
        then do
          pMoveGlobals.pmPML.pmlOrigin .= downO
          pMoveGlobals.pmPML.pmlVelocity .= downV
        else do
          -- Special case
          -- if we were walking along a plane, then we need to copy the Z over
          pMoveGlobals.pmPML.pmlVelocity._z .= (downV^._z)

stepSlideMove_ :: Quake ()
stepSlideMove_ = do
    pml <- use $ pMoveGlobals.pmPML
    let primalVelocity = pml^.pmlVelocity
        timeLeft = pml^.pmlFrameTime

    done <- slideMove (V.replicate Constants.maxClipPlanes (V3 0 0 0)) timeLeft primalVelocity 0 0 4

    unless done $ do
      time <- use $ pMoveGlobals.pmPM.pmState.pmsPMTime
      when (time /= 0) $
        pMoveGlobals.pmPML.pmlVelocity .= primalVelocity

  where slideMove :: V.Vector (V3 Float) -> Float -> V3 Float -> Int -> Int -> Int -> Quake Bool
        slideMove planes timeLeft primalVelocity numPlanes idx maxIdx
          | idx >= maxIdx = return False
          | otherwise = do
              pm <- use $ pMoveGlobals.pmPM
              pml <- use $ pMoveGlobals.pmPML

              let end = (pml^.pmlOrigin) + fmap (* timeLeft) (pml^.pmlVelocity)
              Just traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (pm^.pmMins) (pm^.pmMaxs) end

              if traceT^.tAllSolid -- entity is trapped in another solid
                then do
                  pMoveGlobals.pmPML.pmlVelocity._z .= 0 -- don't build up falling damage
                  return True
                else do
                  numPlanes' <- if (traceT^.tFraction) > 0 -- actually covered some distance
                                  then do
                                    pMoveGlobals.pmPML.pmlOrigin .= (traceT^.tEndPos)
                                    return 0
                                  else
                                    return numPlanes

                  if (traceT^.tFraction) == 1
                    then -- moved the entire distance
                      return False
                    else do
                      -- save entity for contact
                      when ((pm^.pmNumTouch) < Constants.maxTouch && isJust (traceT^.tEnt)) $ do
                        pMoveGlobals.pmPM.pmTouchEnts.ix (pm^.pmNumTouch) .= fromJust (traceT^.tEnt)
                        pMoveGlobals.pmPM.pmNumTouch += 1

                      let timeLeft' = timeLeft - timeLeft * (traceT^.tFraction)

                      -- slide along this plane
                      if numPlanes' >= Constants.maxClipPlanes
                        then do
                          -- this shouldn't really happen
                          v3o <- use $ globals.gVec3Origin
                          pMoveGlobals.pmPML.pmlVelocity .= v3o
                          return False
                        else do
                          let planes' = planes V.// [(numPlanes', traceT^.tPlane.cpNormal)]
                              numPlanes'' = numPlanes' + 1

                          -- modify original_velocity so it parallels all of the clip planes
                          i <- modifyOriginalVelocity planes' 0 numPlanes''

                          done <- if i /= numPlanes''
                            then
                              return False -- go along this plane
                            else do
                              -- go along the crease
                              if numPlanes'' /= 2
                                then do
                                  v3o <- use $ globals.gVec3Origin
                                  pMoveGlobals.pmPML.pmlVelocity .= v3o
                                  return True
                                else do
                                  velocity <- use $ pMoveGlobals.pmPML.pmlVelocity
                                  let dir = (planes' V.! 0) `cross` (planes' V.! 1)
                                      d = dir `dot` velocity
                                  pMoveGlobals.pmPML.pmlVelocity .= fmap (* d) dir
                                  return False

                          if done
                            then
                              return False -- break out of loop
                            else do
                              -- if velocity is against the original velocity, stop dead
                              -- to avoid tiny occilations in sloping corners
                              velocity <- use $ pMoveGlobals.pmPML.pmlVelocity
                              if velocity `dot` primalVelocity <= 0
                                then do
                                  v3o <- use $ globals.gVec3Origin
                                  pMoveGlobals.pmPML.pmlVelocity .= v3o
                                  return False
                                else
                                  slideMove planes' timeLeft' primalVelocity numPlanes'' (idx + 1) maxIdx

        modifyOriginalVelocity :: V.Vector (V3 Float) -> Int -> Int -> Quake Int
        modifyOriginalVelocity planes idx maxIdx
          | idx >= maxIdx = return idx
          | otherwise = do
              velocity <- use $ pMoveGlobals.pmPML.pmlVelocity
              let velocity' = clipVelocity velocity (planes V.! idx) 1.01
              pMoveGlobals.pmPML.pmlVelocity .= velocity'
              let j = checkVelocity planes velocity' idx 0 maxIdx
              if j == maxIdx
                then return idx
                else modifyOriginalVelocity planes (idx + 1) maxIdx

        checkVelocity :: V.Vector (V3 Float) -> V3 Float -> Int -> Int -> Int -> Int
        checkVelocity planes velocity i idx maxIdx
          | idx >= maxIdx = idx
          | i == idx = checkVelocity planes velocity i (idx + 1) maxIdx
          | otherwise = let result = velocity `dot` (planes V.! idx)
                        in if result < 0
                             then idx
                             else checkVelocity planes velocity i (idx + 1) maxIdx

-- Slide off of the impacting object returns the blocked flags (1 = floor, 2 = step / wall)
clipVelocity :: V3 Float -> V3 Float -> Float -> V3 Float
clipVelocity v normal overbounce =
    let backoff = (v `dot` normal) * overbounce
        change = fmap (* backoff) normal
        V3 a b c = v - change
        a' = if a > negate Constants.moveStopEpsilon && a < Constants.moveStopEpsilon then 0 else a
        b' = if b > negate Constants.moveStopEpsilon && b < Constants.moveStopEpsilon then 0 else b
        c' = if c > negate Constants.moveStopEpsilon && c < Constants.moveStopEpsilon then 0 else c
    in V3 a' b' c'

checkJump :: Quake ()
checkJump = do
    pm <- use $ pMoveGlobals.pmPM

    if | (pm^.pmState.pmsPMFlags) .&. pmfTimeLand /= 0 ->
           -- hasn't been long enough since landing to jump again
           return ()

       | (pm^.pmCmd.ucUpMove) < 10 -> -- not holding jump
           pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.&. (complement pmfJumpHeld))

       | (pm^.pmState.pmsPMFlags) .&. pmfJumpHeld /= 0 ->
           -- must wait for jump to be released
           return ()

       | (pm^.pmState.pmsPMType) == Constants.pmDead ->
           return ()

       | (pm^.pmWaterLevel) >= 2 -> do
           -- swimming, not jumping
           pMoveGlobals.pmPM.pmGroundEntity .= Nothing

           pml <- use $ pMoveGlobals.pmPML

           unless ((pml^.pmlVelocity._z) <= -300) $ do
             let v = if | (pm^.pmWaterType) == Constants.contentsWater -> 100
                        | (pm^.pmWaterType) == Constants.contentsSlime -> 80
                        | otherwise -> 50
             pMoveGlobals.pmPML.pmlVelocity._z .= v

       | isNothing (pm^.pmGroundEntity) ->
           -- in air, so no effect
           return ()

       | otherwise -> do
           pml <- use $ pMoveGlobals.pmPML
           let v = (pml^.pmlVelocity._z) + 270
               v' = if v < 270 then 270 else v

           zoom pMoveGlobals $ do
             pmPM.pmState.pmsPMFlags %= (.|. pmfJumpHeld)
             pmPM.pmGroundEntity .= Nothing
             pmPML.pmlVelocity._z .= v'

-- Handles both ground friction and water friction.
friction :: Quake ()
friction = do
    pml <- use $ pMoveGlobals.pmPML

    let speed = norm (pml^.pmlVelocity)

    if speed < 1
      then
        pMoveGlobals.pmPML.pmlVelocity .= V3 0 0 (pml^.pmlVelocity._z)
      else do
        pm <- use $ pMoveGlobals.pmPM
        f <- use $ pMoveGlobals.pmFriction
        stopSpeed <- use $ pMoveGlobals.pmStopSpeed

        -- apply ground friction
        let (fric, control, drop) = if isJust (pm^.pmGroundEntity) && isJust (pml^.pmlGroundSurface) && (((fromJust (pml^.pmlGroundSurface))^.csFlags) .&. Constants.surfSlick) == 0
                                      then
                                        let control = if speed < stopSpeed then stopSpeed else speed
                                            drop = control * f * (pml^.pmlFrameTime)
                                        in (f, control, drop)
                                      else
                                        (0, 0, 0)

        -- apply water friction
        waterFriction <- use $ pMoveGlobals.pmWaterFriction
        let drop' = if (pm^.pmWaterLevel) /= 0 && not (pml^.pmlLadder)
                      then drop + speed * waterFriction * (fromIntegral $ pm^.pmWaterLevel) * (pml^.pmlFrameTime)
                      else drop

        -- scale the velocity
        let newSpeed = if speed - drop' < 0 then 0 else (speed - drop') / speed

        pMoveGlobals.pmPML.pmlVelocity %= (fmap (* newSpeed))

waterMove :: Quake ()
waterMove = do
    pm <- use $ pMoveGlobals.pmPM
    pml <- use $ pMoveGlobals.pmPML
    waterSpeed <- use $ pMoveGlobals.pmWaterSpeed

    -- user intentions
    let wishVel = let vel = fmap (* (fromIntegral $ pm^.pmCmd.ucForwardMove)) (pml^.pmlForward) + fmap (* (fromIntegral $ pm^.pmCmd.ucSideMove)) (pml^.pmlRight)
                  in if (pm^.pmCmd.ucForwardMove) == 0 && (pm^.pmCmd.ucSideMove) == 0 && (pm^.pmCmd.ucUpMove) == 0
                       then vel & _z -~ 60 -- drift towards bottom
                       else vel & _z +~ fromIntegral (pm^.pmCmd.ucUpMove)
        wishVel' = addCurrents pm pml wishVel waterSpeed
        wishDir = normalize wishVel'
        wishSpeed = norm wishVel'

    maxSpeed <- use $ pMoveGlobals.pmMaxSpeed

    let (wishVel'', wishSpeed') = if wishSpeed > maxSpeed
                                    then (fmap (* (maxSpeed / wishSpeed)) wishVel', maxSpeed * 0.5)
                                    else (wishVel', wishSpeed * 0.5)

    accel <- use $ pMoveGlobals.pmWaterAccelerate
    accelerate wishDir wishSpeed' accel

    stepSlideMove

airMove :: Quake ()
airMove = do
    pm <- use $ pMoveGlobals.pmPM
    pml <- use $ pMoveGlobals.pmPML
    waterSpeed <- use $ pMoveGlobals.pmWaterSpeed

    let fmove = fromIntegral $ pm^.pmCmd.ucForwardMove
        smove = fromIntegral $ pm^.pmCmd.ucSideMove
        wishVel = V3 ((pml^.pmlForward._x) * fmove + (pml^.pmlRight._x) * smove) ((pml^.pmlForward._y) * fmove + (pml^.pmlRight._y) * smove) 0
        wishVel' = addCurrents pm pml wishVel waterSpeed
        wishDir = normalize wishVel'
        wishSpeed = norm wishVel'

    -- clamp to server defined max speed
    maxSpeed <- if (pm^.pmState.pmsPMFlags) .&. pmfDucked /= 0
                  then use $ pMoveGlobals.pmDuckSpeed
                  else use $ pMoveGlobals.pmMaxSpeed

    let (wishVel'', wishSpeed') = if wishSpeed > maxSpeed
                                    then (fmap (* (maxSpeed / wishSpeed)) wishVel', maxSpeed)
                                    else (wishVel', wishSpeed)

    if | pml^.pmlLadder -> do
           accel <- use $ pMoveGlobals.pmAccelerate
           accelerate wishDir wishSpeed' accel

           when ((wishVel''^._z) == 0) $
             if (pml^.pmlVelocity._z) > 0
               then do
                 pMoveGlobals.pmPML.pmlVelocity._z -= (fromIntegral $ pm^.pmState.pmsGravity) * (pml^.pmlFrameTime)
                 velocityZ <- use $ pMoveGlobals.pmPML.pmlVelocity._z
                 when (velocityZ < 0) $
                   pMoveGlobals.pmPML.pmlVelocity._z .= 0
               else do
                 pMoveGlobals.pmPML.pmlVelocity._z += (fromIntegral $ pm^.pmState.pmsGravity) * (pml^.pmlFrameTime)
                 velocityZ <- use $ pMoveGlobals.pmPML.pmlVelocity._z
                 when (velocityZ > 0) $
                   pMoveGlobals.pmPML.pmlVelocity._z .= 0

           stepSlideMove

       | isJust (pm^.pmGroundEntity) -> do -- walking on ground
           pMoveGlobals.pmPML.pmlVelocity._z .= 0 -- !!! this is before the accel
           accel <- use $ pMoveGlobals.pmAccelerate
           accelerate wishDir wishSpeed' accel

           if (pm^.pmState.pmsGravity) > 0
             then pMoveGlobals.pmPML.pmlVelocity._z .= 0 -- !!! this is before the accel
             else pMoveGlobals.pmPML.pmlVelocity._z -= (fromIntegral $ pm^.pmState.pmsGravity) * (pml^.pmlFrameTime)

           velocity <- use $ pMoveGlobals.pmPML.pmlVelocity
           unless ((velocity^._x) == 0 && (velocity^._y) == 0) $
             stepSlideMove

       | otherwise -> do -- not on ground, so little effect on velocity
           airAccel <- use $ pMoveGlobals.pmAirAccelerate
           if airAccel /= 0
             then do
               accel <- use $ pMoveGlobals.pmAccelerate
               airAccelerate wishDir wishSpeed' accel
             else
               accelerate wishDir wishSpeed' 1

           -- add gravity
           pMoveGlobals.pmPML.pmlVelocity._z -= (fromIntegral $ pm^.pmState.pmsGravity) * (pml^.pmlFrameTime)
           stepSlideMove

addCurrents :: PMoveT -> PmlT -> V3 Float -> Float -> V3 Float
addCurrents pm pml (V3 a b c) waterSpeed =
    -- account for ladders
    let wishVel = if (pml^.pmlLadder) && abs(pml^.pmlVelocity._z) <= 200
                    then let c' = if | (pm^.pmViewAngles.(Math3D.v3Access Constants.pitch)) <= -15 && (pm^.pmCmd.ucForwardMove) > 0 -> 200
                                     | (pm^.pmViewAngles.(Math3D.v3Access Constants.pitch)) >= 15 && (pm^.pmCmd.ucForwardMove) > 0 -> -200
                                     | (pm^.pmCmd.ucUpMove) > 0 -> 200
                                     | (pm^.pmCmd.ucUpMove) < 0 -> -200
                                     | otherwise -> 0

                             -- limit horizontal speed when on a ladder
                             a' = if | a < -25 -> -25
                                     | a > 25 -> 25
                                     | otherwise -> a

                             b' = if | b < -25 -> -25
                                     | b > 25 -> 25
                                     | otherwise -> b

                         in V3 a' b' c'
                    else V3 a b c

        -- add water currents
        v = if (pm^.pmWaterType) .&. Constants.maskCurrent /= 0
              then let va = if (pm^.pmWaterType) .&. Constants.contentsCurrent0 /= 0
                              then 1 else 0
                       vb = if (pm^.pmWaterType) .&. Constants.contentsCurrent90 /= 0
                              then 1 else 0
                       vc = if (pm^.pmWaterType) .&. Constants.contentsCurrent180 /= 0
                              then -1 else 0
                       vd = if (pm^.pmWaterType) .&. Constants.contentsCurrent270 /= 0
                              then -1 else 0
                       ve = if (pm^.pmWaterType) .&. Constants.contentsCurrentUp /= 0
                              then 1 else 0
                       vf = if (pm^.pmWaterType) .&. Constants.contentsCurrentDown /= 0
                              then -1 else 0
                   in V3 (va + vc) (vb + vd) (ve + vf)
              else V3 0 0 0

        s = if (pm^.pmWaterLevel) == 1 && isJust (pm^.pmGroundEntity)
              then waterSpeed / 2 else waterSpeed

        wishVel' = wishVel + fmap (* s) v

        -- add conveyor belt velocities
        v' = if isJust (pm^.pmGroundEntity)
               then let va = if (pml^.pmlGroundContents) .&. Constants.contentsCurrent0 /= 0
                               then 1 else 0
                        vb = if (pml^.pmlGroundContents) .&. Constants.contentsCurrent90 /= 0
                               then 1 else 0
                        vc = if (pml^.pmlGroundContents) .&. Constants.contentsCurrent180 /= 0
                               then -1 else 0
                        vd = if (pml^.pmlGroundContents) .&. Constants.contentsCurrent270 /= 0
                               then -1 else 0
                        ve = if (pml^.pmlGroundContents) .&. Constants.contentsCurrentUp /= 0
                               then 1 else 0
                        vf = if (pml^.pmlGroundContents) .&. Constants.contentsCurrentDown /= 0
                               then -1 else 0
                    in V3 (va + vc) (vb + vd) (ve + vf)
               else V3 0 0 0

        wishVel'' = wishVel' + fmap (* 100) v'

    in wishVel''

-- Handles user intended acceleration
accelerate :: V3 Float -> Float -> Float -> Quake ()
accelerate wishDir wishSpeed accel = do
    pml <- use $ pMoveGlobals.pmPML

    let currentSpeed = (pml^.pmlVelocity) `dot` wishDir
        addSpeed = wishSpeed - currentSpeed

    unless (addSpeed <= 0) $ do
      let accelSpeed = accel * (pml^.pmlFrameTime) * wishSpeed
          accelSpeed' = if accelSpeed > addSpeed
                          then addSpeed
                          else accelSpeed

      pMoveGlobals.pmPML.pmlVelocity += (fmap (* accelSpeed') wishDir)

airAccelerate :: V3 Float -> Float -> Float -> Quake ()
airAccelerate wishDir wishSpeed accel = do
    pml <- use $ pMoveGlobals.pmPML

    let wishSpd = if wishSpeed > 30 then 30 else wishSpeed
        currentSpeed = (pml^.pmlVelocity) `dot` wishDir
        addSpeed = wishSpd - currentSpeed

    unless (addSpeed <= 0) $ do
      let accelSpeed = accel * wishSpeed * (pml^.pmlFrameTime)
          accelSpeed' = if accelSpeed > addSpeed
                          then addSpeed
                          else accelSpeed

      pMoveGlobals.pmPML.pmlVelocity += (fmap (* accelSpeed') wishDir)
