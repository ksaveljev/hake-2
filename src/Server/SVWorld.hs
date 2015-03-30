module Server.SVWorld where

import Control.Lens ((.=), (^.), preuse, ix)
import Control.Monad (void)
import Linear.V3 (V3)
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.TraceT
import Server.AreaNodeT
import qualified Constants

initNodes :: Quake ()
initNodes = svGlobals.svAreaNodes .= V.generate Constants.areaNodes newAreaNodeT

{-
- =============== SV_CreateAreaNode
- 
- Builds a uniformly subdivided tree for the given world size
- ===============
-}
createAreaNode :: Int -> V3 Float -> V3 Float -> Quake Int -- returns index of svGlobals.svAreaNodes
createAreaNode _ _ _ = io (putStrLn "SVWorld.createAreaNode") >> undefined -- TODO

unlinkEdict :: EdictT -> Quake ()
unlinkEdict _ = io (putStrLn "SVWorld.unlinkEdict") >> undefined -- TODO

linkEdict :: EdictT -> Quake ()
linkEdict _ = io (putStrLn "SVWorld.linkEdict") >> undefined -- TODO

areaEdicts :: V3 Float -> V3 Float -> V.Vector EdictT -> Int -> Int -> Quake Int
areaEdicts _ _ _ _ _ = io (putStrLn "SVWorld.areaEdicts") >> undefined -- TODO

trace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> Quake TraceT
trace _ _ _ _ _ _ = io (putStrLn "SVWorld.trace") >> undefined -- TODO

clearWorld :: Quake ()
clearWorld = do
    initNodes

    svGlobals.svNumAreaNodes .= 0

    Just model <- preuse $ svGlobals.svServer.sModels.ix 1

    void $ createAreaNode 0 (model^.cmMins) (model^.cmMaxs)
