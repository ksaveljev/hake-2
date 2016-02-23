{-# LANGUAGE Rank2Types #-}
module Server.SVWorld
  ( areaEdicts
  , linkEdict
  , trace
  , unlinkEdict
  ) where

import           Types

import           Control.Lens (Lens')
import qualified Data.Vector as V
import           Linear (V3)

trace :: V3 Float -> Maybe (V3 Float) -> Maybe (V3 Float) -> V3 Float -> Maybe EdictRef -> Int -> Quake TraceT
trace = error "SVWorld.trace" -- TODO

unlinkEdict :: EdictRef -> Quake ()
unlinkEdict = error "SVWorld.unlinkEdict" -- TODO

linkEdict :: EdictRef -> Quake ()
linkEdict = error "SVWorld.linkEdict" -- TODO

areaEdicts :: V3 Float -> V3 Float -> Lens' QuakeState (V.Vector EdictRef) -> Int -> Int -> Quake Int
areaEdicts = error "SVWorld.areaEdicts" -- TODO