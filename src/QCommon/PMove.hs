{-# LANGUAGE MultiWayIf #-}
module QCommon.PMove where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), (+=))
import Control.Monad (when, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _x, _y, _z, normalize, norm)
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified Constants
import qualified Util.Math3D as Math3D

-- try all single bits first
jitterBits :: UV.Vector Int
jitterBits = UV.fromList [ 0, 4, 1, 2, 3, 5, 6, 7 ]

offset :: V3 Int
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
            io (putStrLn "PMove.pMove#dropTimingCounter") >> undefined -- TODO

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

    let (Just forward, Just right, Just up) = Math3D.angleVectors (pm'^.pmViewAngles) True True True
    
    zoom (pMoveGlobals.pmPML) $ do
      pmlForward .= forward
      pmlRight .= right
      pmlUp .= up

flyMove :: Bool -> Quake ()
flyMove _ = do
    io (putStrLn "PMove.flyMove") >> undefined -- TODO

snapPosition :: Quake ()
snapPosition = do
    io (putStrLn "PMove.snapPosition") >> undefined -- TODO

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

initialSnapPosition :: Quake ()
initialSnapPosition = do
    io (putStrLn "PMove.initialSnapPosition") >> undefined -- TODO

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
            when ((pm^.pmState.pmsPMFlags) .&. pmfTimeWaterJump /= 0) $
              zoom (pMoveGlobals.pmPM.pmState) $ do
                pmsPMFlags %= (.&. (complement (pmfTimeWaterJump .|. pmfTimeLand .|. pmfTimeTeleport)))
                pmsPMTime .= 0

            pm' <- use $ pMoveGlobals.pmPM

            when ((pm'^.pmState.pmsPMFlags) .&. pmfOnGround /= 0) $ do
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

deadMove :: Quake ()
deadMove = do
    io (putStrLn "PMove.deadMove") >> undefined -- TODO

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

stepSlideMove :: Quake ()
stepSlideMove = do
    io (putStrLn "PMove.stepSlideMove") >> undefined -- TODO

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
    io (putStrLn "PMove.waterMove") >> undefined -- TODO

airMove :: Quake ()
airMove = do
    io (putStrLn "PMove.airMove") >> undefined -- TODO
