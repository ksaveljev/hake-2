{-# LANGUAGE OverloadedStrings #-}
module Client.M where

import Control.Lens (zoom, (.=), preuse, ix, (^.), use, (%=), (+=), (%~))
import Control.Monad (unless, when)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isNothing)
import Linear (V3(..), _z)

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

checkBottom :: EdictReference -> Quake Bool
checkBottom _ = do
    io (putStrLn "M.checkBottom") >> undefined -- TODO

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
