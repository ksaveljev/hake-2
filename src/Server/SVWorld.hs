{-# LANGUAGE Rank2Types #-}
module Server.SVWorld
  ( areaEdicts
  , clearWorld
  , linkEdict
  , pointContents
  , trace
  , unlinkEdict
  ) where

import qualified Constants
import           Game.CModelT
import           Game.LinkT
import           QuakeRef
import           QuakeState
import           Server.AreaNodeT
import           Server.ServerT
import           Types

import           Control.Lens (Lens', use, preuse, ix, (^.), (.=), (+=), (&), (.~))
import           Control.Monad (void)
import qualified Data.Vector as V
import           Linear (V3, _x, _y)

trace :: V3 Float -> Maybe (V3 Float) -> Maybe (V3 Float) -> V3 Float -> Maybe (Ref EdictT) -> Int -> Quake TraceT
trace = error "SVWorld.trace" -- TODO

unlinkEdict :: Ref EdictT -> Quake ()
unlinkEdict = error "SVWorld.unlinkEdict" -- TODO

linkEdict :: Ref EdictT -> Quake ()
linkEdict = error "SVWorld.linkEdict" -- TODO

areaEdicts :: V3 Float -> V3 Float -> Lens' QuakeState (V.Vector (Ref EdictT)) -> Int -> Int -> Quake Int
areaEdicts = error "SVWorld.areaEdicts" -- TODO

clearWorld :: Quake ()
clearWorld =
  do initNodes
     svGlobals.svNumAreaNodes .= 0
     modelRef <- preuse (svGlobals.svServer.sModels.ix 1)
     maybe modelRefError proceedClear modelRef
  where modelRefError = error "SVWorld.clearWorld modelRef is Nothing"
        proceedClear modelRef =
          do model <- readRef modelRef
             void (createAreaNode 0 (model^.cmMins) (model^.cmMaxs))

pointContents :: V3 Float -> Quake Int
pointContents = error "SVWorld.pointContents" -- TODO

createAreaNode :: Int -> V3 Float -> V3 Float -> Quake (Ref AreaNodeT)
createAreaNode depth mins maxs =
  do numAreaNodes <- use (svGlobals.svNumAreaNodes)
     svGlobals.svNumAreaNodes += 1
     areaNode <- readRef (Ref numAreaNodes)
     clearLink (areaNode^.anTriggerEdicts)
     clearLink (areaNode^.anSolidEdicts)
     clearNode numAreaNodes
     return (Ref numAreaNodes)
  where clearNode numAreaNodes
          | depth == Constants.areaDepth =
              modifyRef (Ref numAreaNodes) (\v -> v & anAxis .~ (-1)
                                                    & anChildren .~ (Nothing, Nothing))
          | otherwise = createChildrenAreaNodes (Ref numAreaNodes) depth mins maxs

createChildrenAreaNodes :: Ref AreaNodeT -> Int -> V3 Float -> V3 Float -> Quake ()
createChildrenAreaNodes areaNodeRef depth mins maxs =
  do child1 <- createAreaNode (depth + 1) mins2 maxs2
     child2 <- createAreaNode (depth + 1) mins1 maxs1
     modifyRef areaNodeRef (\v -> v & anAxis .~ axis
                                    & anDist .~ dist
                                    & anChildren .~ (Just child1, Just child2))
  where size = maxs - mins
        axis = if (size^._x) > (size^._y) then 0 else 1 :: Int
        dist = 0.5 * ((if axis == 0 then maxs^._x else maxs^._y) + (if axis == 0 then mins^._x else mins^._y))
        mins1 = mins
        mins2 = if axis == 0 then mins & _x .~ dist else mins & _y .~ dist
        maxs1 = if axis == 0 then maxs & _x .~ dist else maxs & _y .~ dist
        maxs2 = maxs

initNodes :: Quake ()
initNodes = svGlobals.svAreaNodes .= V.generate Constants.areaNodes newAreaNodeT

clearLink :: Ref LinkT -> Quake ()
clearLink linkRef = modifyRef linkRef (\v -> v & lNext .~ Just linkRef
                                               & lPrev .~ Just linkRef)