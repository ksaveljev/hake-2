{-# LANGUAGE OverloadedStrings #-}
module Client.M where

import Control.Lens (zoom, (.=), preuse, ix, (^.), use, (%=), (+=), (%~))
import Control.Monad (unless, when)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isNothing)
import Linear (V3(..), _x, _y, _z)

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import {-# SOURCE #-} qualified Server.SV as SV
import qualified Util.Lib as Lib

checkGround :: EdictReference -> Quake ()
checkGround edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    unless ((edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) /= 0) $ do
      if (edict^.eEdictPhysics.eVelocity._z) > 100
        then
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity .= Nothing
        else do
          -- if the hull point one-quarter unit down is solid the entity is
          -- on ground
          let point = let V3 a b c = edict^.eEntityState.esOrigin in V3 a b (c - 0.25)

          trace <- use $ gameBaseGlobals.gbGameImport.giTrace
          traceT <- trace (edict^.eEntityState.esOrigin) 
                         (Just $ edict^.eEdictMinMax.eMins)
                         (Just $ edict^.eEdictMinMax.eMaxs)
                         point
                         edictRef
                         Constants.maskMonsterSolid

          -- check steepness
          if (traceT^.tPlane.cpNormal._z) < 0.7 && not (traceT^.tStartSolid)
            then
              gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoGroundEntity .= Nothing
            else do
              when (not (traceT^.tStartSolid) && not (traceT^.tAllSolid)) $ do
                let Just (EdictReference traceIdx) = traceT^.tEnt
                Just linkCount <- preuse $ gameBaseGlobals.gbGEdicts.ix traceIdx.eLinkCount

                zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
                  eEntityState.esOrigin .= traceT^.tEndPos
                  eEdictOther.eoGroundEntity .= traceT^.tEnt
                  eGroundEntityLinkCount .= linkCount
                  eEdictPhysics.eVelocity._z .= 0

-- Stops the Flies.
fliesOff :: EntThink
fliesOff =
  GenericEntThink "m_fliesoff" $ \(EdictReference edictIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState) $ do
      esEffects %= (.&. (complement Constants.efFlies))
      esSound .= 0

    return True

-- Starts the Flies as setting the animation flag in the entity.
fliesOn :: EntThink
fliesOn =
  GenericEntThink "m_flies_on" $ \(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    unless ((edict^.eWaterLevel) /= 0) $ do
      time <- use $ gameBaseGlobals.gbLevel.llTime
      soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
      inflies <- soundIndex (Just "infantry/inflies1.wav")

      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
        eEntityState.esEffects %= (.|. Constants.efFlies)
        eEntityState.esSound .= inflies
        eEdictAction.eaThink .= Just fliesOff
        eEdictAction.eaNextThink .= time + 60

    return True

-- Adds some flies after a random time
flyCheck :: EntThink
flyCheck =
  GenericEntThink "m_fly_check" $ \(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    f <- Lib.randomF

    unless ((edict^.eWaterLevel /= 0) || f > 0.5) $ do
      time <- use $ gameBaseGlobals.gbLevel.llTime
      nf <- Lib.randomF

      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction) $ do
        eaThink .= Just fliesOn
        eaNextThink .= time + 5 + 10 * nf

    return True

dropToFloor :: EntThink
dropToFloor =
  GenericEntThink "m_drop_to_floor" $ \edictRef@(EdictReference edictIdx) -> do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin._z += 1
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let end' = let V3 a b c = edict^.eEntityState.esOrigin in V3 a b (c - 256)

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let trace = gameImport^.giTrace
        linkEntity = gameImport^.giLinkEntity

    traceT <- trace (edict^.eEntityState.esOrigin) (Just $ edict^.eEdictMinMax.eMins) (Just $ edict^.eEdictMinMax.eMaxs) end' edictRef Constants.maskMonsterSolid

    if (traceT^.tFraction) == 1 || (traceT^.tAllSolid)
      then
        return True
      else do
        gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin .= traceT^.tEndPos
        linkEntity edictRef
        checkGround edictRef
        catagorizePosition edictRef
        return True

{-
- Returns false if any part of the bottom of the entity is off an edge that
- is not a staircase.
-}
checkBottom :: EdictReference -> Quake Bool
checkBottom edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    let mins = (edict^.eEntityState.esOrigin) + (edict^.eEdictMinMax.eMins)
        maxs = (edict^.eEntityState.esOrigin) + (edict^.eEdictMinMax.eMaxs)

    -- if all of the points under the corners are solid world, don't bother
    -- with the tougher checks
    -- the corners must be within 16 of the midpoint
    done <- doChecks ((mins^._z) - 1) mins maxs 0 2 0 2

    case done of
      Just v -> return v
      Nothing -> do
        gameBaseGlobals.gbCYes += 1
        return True -- we got out easy

  where doChecks :: Float -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake (Maybe Bool)
        doChecks c mins maxs x maxX y maxY
          | x >= maxX = return Nothing
          | y >= maxY = doChecks c mins maxs (x + 1) maxX 0 maxY
          | otherwise = do
              let a = if x /= 0 then maxs^._x else mins^._x
                  b = if y /= 0 then maxs^._y else mins^._y
                  start = V3 a b c

              gameImport <- use $ gameBaseGlobals.gbGameImport
              let pointContents = gameImport^.giPointContents
                  trace = gameImport^.giTrace
              contents <- pointContents start

              if contents /= Constants.contentsSolid
                then do
                  gameBaseGlobals.gbCNo += 1
                  -- check it for real
                  let a' = ((mins^._x) + (maxs^._x)) * 0.5
                      b' = ((mins^._y) + (maxs^._y)) * 0.5
                      start' = V3 a' b' (mins^._z)
                      stop' = V3 a' b' ((mins^._z) - 2 * (fromIntegral Constants.stepSize))

                  v3o <- use $ globals.vec3Origin
                  traceT <- trace start' (Just v3o) (Just v3o) stop' edictRef Constants.maskMonsterSolid

                  if (traceT^.tFraction) == 1
                    then
                      return (Just False)
                    else do
                      let mid = traceT^.tEndPos._z
                          bottom = traceT^.tEndPos._z

                      -- the corners must be withing 16 of the midpoint
                      done <- checkCorners mins maxs start' stop' mid bottom 0 2 0 2

                      case done of
                        Just _ -> return done
                        Nothing -> do
                          gameBaseGlobals.gbCYes += 1
                          return (Just True)

                else
                  doChecks c mins maxs x maxX (y + 1) maxY

        checkCorners :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Float -> Float -> Int -> Int -> Int -> Int -> Quake (Maybe Bool)
        checkCorners mins maxs start stop mid bottom x maxX y maxY
          | x >= maxX = return Nothing
          | y >= maxY = checkCorners mins maxs start stop mid bottom (x + 1) maxX 0 maxY
          | otherwise = do
              let a = if x /= 0 then maxs^._x else mins^._x
                  b = if y /= 0 then maxs^._y else mins^._y
                  start' = V3 a b (start^._z)
                  stop' = V3 a b (stop^._z)

              v3o <- use $ globals.vec3Origin
              trace <- use $ gameBaseGlobals.gbGameImport.giTrace
              traceT <- trace start' (Just v3o) (Just v3o) stop' edictRef Constants.maskMonsterSolid

              let bottom' = if (traceT^.tFraction) /= 1 && (traceT^.tEndPos._z) > bottom
                              then traceT^.tEndPos._z
                              else bottom

              if (traceT^.tFraction) == 1 || mid - (traceT^.tEndPos._z) > fromIntegral Constants.stepSize
                then return (Just False)
                else checkCorners mins maxs start' stop' mid bottom' x maxX (y + 1) maxY

walkMove :: EdictReference -> Float -> Float -> Quake Bool
walkMove edictRef@(EdictReference edictIdx) yaw dist = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    if isNothing (edict^.eEdictOther.eoGroundEntity) && (edict^.eFlags) .&. (Constants.flFly .|. Constants.flSwim) == 0
      then
        return False
      else do
        let yaw' = yaw * pi * 2 / 360
            move = V3 ((cos yaw') * dist) ((sin yaw') * dist) 0
        SV.moveStep edictRef move True


catagorizePosition :: EdictReference -> Quake ()
catagorizePosition (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    pointContents <- use $ gameBaseGlobals.gbGameImport.giPointContents

    let point = let V3 a b c = edict^.eEntityState.esOrigin in V3 a b (c + (edict^.eEdictMinMax.eMins._z) + 1)
    cont <- pointContents point

    if cont .&. Constants.maskWater == 0
      then do
        gameBaseGlobals.gbGEdicts.ix edictIdx.eWaterLevel .= 0
        gameBaseGlobals.gbGEdicts.ix edictIdx.eWaterType .= 0
      else do
        gameBaseGlobals.gbGEdicts.ix edictIdx.eWaterLevel .= 1
        gameBaseGlobals.gbGEdicts.ix edictIdx.eWaterType .= cont

        let point' = _z %~ (+26) $ point
        cont' <- pointContents point'

        unless (cont' .&. Constants.maskWater == 0) $ do
          gameBaseGlobals.gbGEdicts.ix edictIdx.eWaterLevel .= 2
          let point'' = _z %~ (+22) $ point'
          cont'' <- pointContents point''

          when (cont'' .&. Constants.maskWater /= 0) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eWaterLevel .= 3
