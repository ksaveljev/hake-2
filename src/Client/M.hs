{-# LANGUAGE OverloadedStrings #-}
module Client.M where

import Control.Lens (zoom, (.=), preuse, ix, (^.), use, (%=))
import Control.Monad (unless, when)
import Data.Bits ((.|.), (.&.), complement)
import Linear (V3(..), _z)

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
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
  GenericEntThink "m_drop_to_floor" $ \_ -> do
    io (putStrLn "M.dropToFloor") >> undefined -- TODO

checkBottom :: EdictReference -> Quake Bool
checkBottom _ = do
    io (putStrLn "M.checkBottom") >> undefined -- TODO

walkMove :: EdictReference -> Float -> Float -> Quake Bool
walkMove _ _ _ = do
    io (putStrLn "M.walkMove") >> undefined -- TODO
