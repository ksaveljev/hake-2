module Server.SVWorld where

import Linear.V3 (V3)
import qualified Data.Vector as V

import Quake
import Game.EdictT
import Game.TraceT

unlinkEdict :: EdictT -> Quake ()
unlinkEdict = undefined -- TODO

linkEdict :: EdictT -> Quake ()
linkEdict = undefined -- TODO

areaEdicts :: V3 Float -> V3 Float -> V.Vector EdictT -> Int -> Int -> Quake Int
areaEdicts = undefined -- TODO

trace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> Quake TraceT
trace = undefined -- TODO

clearWorld :: Quake ()
clearWorld = undefined -- TODO
