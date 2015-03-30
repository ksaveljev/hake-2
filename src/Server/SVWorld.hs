module Server.SVWorld where

import Control.Lens ((.=), (^.), (+=), use, preuse, ix, set)
import Control.Monad (void)
import Linear.V3 (V3, _x, _y)
import qualified Data.Vector as V

import Quake
import QuakeState
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
createAreaNode depth mins maxs = do
    numAreaNodes <- use $ svGlobals.svNumAreaNodes
    svGlobals.svNumAreaNodes += 1

    Just areaNode <- preuse $ svGlobals.svAreaNodes.ix numAreaNodes
    clearLink (areaNode^.anTriggerEdicts)
    clearLink (areaNode^.anSolidEdicts)

    if depth == Constants.areaDepth
      then
        svGlobals.svAreaNodes.ix numAreaNodes .= areaNode { _anAxis = -1, _anChildren = (Nothing, Nothing) }
      else do
        let size = maxs - mins -- V3 Float
            axis = if (size^._x) > (size^._y) then (0 :: Int) else (1 :: Int)
            dist = 0.5 * ((if axis == 0 then maxs^._x else maxs^._y) + (if axis == 0 then mins^._x else mins^._y))
            mins1 = mins
            mins2 = if axis == 0 then set _x dist mins else set _y dist mins
            maxs1 = if axis == 0 then set _x dist maxs else set _y dist maxs
            maxs2 = maxs

        child1 <- createAreaNode (depth + 1) mins2 maxs2
        child2 <- createAreaNode (depth + 1) mins1 maxs1

        svGlobals.svAreaNodes.ix numAreaNodes .= areaNode { _anAxis = axis, _anDist = dist, _anChildren = (Just child1, Just child2) }

    return numAreaNodes

-- ClearLink is used for new headnodes
clearLink :: Int -> Quake () -- int is index of areanode from svGlobals.svLinks
clearLink idx = do
    Just link <- preuse $ svGlobals.svLinks.ix idx
    svGlobals.svLinks.ix idx .= link { _lNext = Just idx, _lPrev = Just idx }

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

    Just modelIdx <- preuse $ svGlobals.svServer.sModels.ix 1
    Just model <- preuse $ cmGlobals.cmMapCModels.ix modelIdx

    void $ createAreaNode 0 (model^.cmMins) (model^.cmMaxs)
