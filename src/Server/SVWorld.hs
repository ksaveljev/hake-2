{-# LANGUAGE Rank2Types #-}
module Server.SVWorld
  ( areaEdicts
  , clearWorld
  , linkEdict
  , pointContents
  , trace
  , unlinkEdict
  ) where

import           Types

import           Control.Lens (Lens')
import qualified Data.Vector as V
import           Linear (V3)

trace :: V3 Float -> Maybe (V3 Float) -> Maybe (V3 Float) -> V3 Float -> Maybe (Ref EdictT) -> Int -> Quake TraceT
trace = error "SVWorld.trace" -- TODO

unlinkEdict :: Ref EdictT -> Quake ()
unlinkEdict = error "SVWorld.unlinkEdict" -- TODO

linkEdict :: Ref EdictT -> Quake ()
linkEdict = error "SVWorld.linkEdict" -- TODO

areaEdicts :: V3 Float -> V3 Float -> Lens' QuakeState (V.Vector (Ref EdictT)) -> Int -> Int -> Quake Int
areaEdicts = error "SVWorld.areaEdicts" -- TODO

clearWorld :: Quake ()
clearWorld = error "SVWorld.clearWorld" -- TODO

pointContents :: V3 Float -> Quake Int
pointContents = error "SVWorld.pointContents" -- TODO