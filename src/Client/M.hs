{-# LANGUAGE OverloadedStrings #-}
module Client.M where

import Control.Lens (zoom, (.=), preuse, ix, (^.), use, (%=))
import Control.Monad (unless)
import Data.Bits ((.|.), (.&.), complement)

import QuakeState
import Game.Adapters
import qualified Constants
import qualified Util.Lib as Lib

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
      inflies <- soundIndex "infantry/inflies1.wav"

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
