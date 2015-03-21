module Server.SVWorld where

import Linear.V3 (V3)
import qualified Data.Vector as V

import Quake
import Game.EdictT
import Game.TraceT

unlinkEdict :: EdictT -> Quake ()
unlinkEdict _ = io (putStrLn "SVWorld.unlinkEdict") >> undefined -- TODO

linkEdict :: EdictT -> Quake ()
linkEdict _ = io (putStrLn "SVWorld.linkEdict") >> undefined -- TODO

areaEdicts :: V3 Float -> V3 Float -> V.Vector EdictT -> Int -> Int -> Quake Int
areaEdicts _ _ _ _ _ = io (putStrLn "SVWorld.areaEdicts") >> undefined -- TODO

trace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> Quake TraceT
trace _ _ _ _ _ _ = io (putStrLn "SVWorld.trace") >> undefined -- TODO

clearWorld :: Quake ()
clearWorld = io (putStrLn "SVWorld.clearWorld") >> undefined -- TODO
