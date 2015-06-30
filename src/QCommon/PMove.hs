{-# LANGUAGE MultiWayIf #-}
module QCommon.PMove where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), (+=))
import Control.Monad (when, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear.V3 (V3(..), _x, _y, _z)
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

            io (putStrLn "PMove.pMove") >> undefined -- TODO

clampAngles :: Quake ()
clampAngles = do
    pm <- use $ pMoveGlobals.pmPM

    let pm' = if fromIntegral (pm^.pmState.pmsPMFlags) .&. pmfTimeTeleport /= 0
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
                      return pm { _pmState = (pm^.pmState) { _pmsPMFlags = (pm^.pmState.pmsPMFlags) .|. (fromIntegral pmfDucked)} }

                  | (pm^.pmCmd.ucUpMove) < 0 && (fromIntegral (pm^.pmState.pmsPMFlags) .&. pmfOnGround /= 0) -> -- duck
                      return pm { _pmState = (pm^.pmState) { _pmsPMFlags = (pm^.pmState.pmsPMFlags) .|. (fromIntegral pmfDucked)} }

                  | otherwise -> do -- stand up if possible
                      if fromIntegral (pm^.pmState.pmsPMFlags) .&. pmfDucked /= 0
                        then do
                          pml <- use $ pMoveGlobals.pmPML
                          Just traceT <- (pm^.pmTrace) (pml^.pmlOrigin) (V3 (-16) (-16) (-24)) (V3 16 16 32) (pml^.pmlOrigin)
                          if traceT^.tAllSolid
                            then return pm
                            else return pm { _pmState = (pm^.pmState) { _pmsPMFlags = (pm^.pmState.pmsPMFlags) .&. (complement (fromIntegral pmfDucked))} }
                        else
                          return pm

        if fromIntegral (pm'^.pmState.pmsPMFlags) .&. pmfDucked /= 0
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
          pmState.pmsPMFlags %= (.&. (complement (fromIntegral pmfOnGround)))
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
              pmState.pmsPMFlags %= (.&. (complement (fromIntegral pmfOnGround)))
          else do
            when (fromIntegral (pm^.pmState.pmsPMFlags) .&. pmfTimeWaterJump /= 0) $
              zoom (pMoveGlobals.pmPM.pmState) $ do
                pmsPMFlags %= (.&. (complement (fromIntegral (pmfTimeWaterJump .|. pmfTimeLand .|. pmfTimeTeleport))))
                pmsPMTime .= 0

            pm' <- use $ pMoveGlobals.pmPM

            when (fromIntegral (pm'^.pmState.pmsPMFlags) .&. pmfOnGround /= 0) $ do
              -- just hit the ground
              pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.|. (fromIntegral pmfOnGround))

              -- don't do landing time if we were just going down a slope
              when ((pml^.pmlVelocity._z) < -200) $ do
                pMoveGlobals.pmPM.pmState.pmsPMFlags %= (.|. (fromIntegral pmfTimeLand))
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
    io (putStrLn "PMove.checkSpecialMovement") >> undefined -- TODO
