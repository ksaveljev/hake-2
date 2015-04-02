{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.SVWorld where

import Control.Lens ((.=), (^.), (-=), (+=), use, preuse, ix, set, zoom)
import Control.Monad (void, unless, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Maybe (isNothing, isJust)
import Linear.V3 (V3, _x, _y, _z)
import qualified Data.Foldable as F
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
clearLink :: LinkReference -> Quake ()
clearLink lr@(LinkReference idx) = do
    Just link <- preuse $ svGlobals.svLinks.ix idx
    svGlobals.svLinks.ix idx .= link { _lNext = Just lr, _lPrev = Just lr }

removeLink :: LinkReference -> Quake ()
removeLink (LinkReference idx) = do
    Just link <- preuse $ svGlobals.svLinks.ix idx
    let Just (LinkReference nextLinkIdx) = link^.lNext
        Just (LinkReference prevLinkIdx) = link^.lPrev
    svGlobals.svLinks.ix nextLinkIdx.lPrev .= link^.lPrev
    svGlobals.svLinks.ix prevLinkIdx.lNext .= link^.lNext

unlinkEdict :: EdictReference -> Quake ()
unlinkEdict (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let lr@(LinkReference linkIdx) = edict^.eArea

    Just link <- preuse $ svGlobals.svLinks.ix linkIdx
    unless (isNothing (link^.lPrev)) $ do
      removeLink lr
      svGlobals.svLinks.ix linkIdx .= link { _lNext = Nothing, _lPrev = Nothing }

linkEdict :: EdictReference -> Quake ()
linkEdict er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let LinkReference linkIdx = edict^.eArea
    Just link <- preuse $ svGlobals.svLinks.ix linkIdx

    when (isJust (link^.lPrev)) $
      unlinkEdict er

    -- don't add the world and the one not in use
    unless (edictIdx == 0 || not (edict^.eInUse)) $ do
      -- set the size
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictMinMax.eSize .= (edict^.eEdictMinMax.eMaxs) - (edict^.eEdictMinMax.eMins)

      -- encode the size into the entity_state for client prediction
      if | (edict^.eSolid) == Constants.solidBbox && 0 == ((edict^.eSvFlags) .&. Constants.svfDeadMonster) -> do
                 -- assume that x/y are equal and symetric
             let i :: Int = truncate ((edict^.eEdictMinMax.eMaxs._x) / 8)
                 -- z is not symetric
                 j :: Int = truncate $ ((edict^.eEdictMinMax.eMins._z) / (-8))
                 -- and z maxs can be negative
                 k :: Int= truncate $ ((32 + (edict^.eEdictMinMax.eMaxs._z)) / 8)

                 i' = if | i < 1 -> 1
                         | i > 31 -> 31
                         | otherwise -> i

                 j' = if | j < 1 -> 1
                         | j > 31 -> 31
                         | otherwise -> j

                 k' = if | k < 1 -> 1
                         | k > 63 -> 63
                         | otherwise -> k

             gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSolid .= (k' `shiftL` 10) .|. (j' `shiftL` 5) .|. i'
         | (edict^.eSolid) == Constants.solidBsp ->
             -- a solid _bbox will never create this value
             gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSolid .= 31
         | otherwise ->
             gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSolid .= 0

      -- set the abs box
      if (edict^.eSolid) == Constants.solidBsp && F.any (/= 0) (edict^.eEntityState.esAngles)
        then do
          -- expand for rotation
          let aMins = fmap abs (edict^.eEdictMinMax.eMins)
              aMaxs = fmap abs (edict^.eEdictMinMax.eMaxs)
              minMax = F.maximum aMins
              maxMax = F.maximum aMaxs
              m = F.maximum [minMax, maxMax]

          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictMinMax) $ do
            eAbsMin .= fmap ((-) m) (edict^.eEntityState.esOrigin)
            eAbsMax .= fmap (+ m) (edict^.eEntityState.esOrigin)

        else do
          -- normal
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictMinMax) $ do
            eAbsMin .= (edict^.eEntityState.esOrigin) + (edict^.eEdictMinMax.eMins)
            eAbsMax .= (edict^.eEntityState.esOrigin) + (edict^.eEdictMinMax.eMaxs)

      -- because movement is clipped an epsilon away from an actual edge,
      -- we must fully check even when bouding boxes don't quite touch
      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
        eEdictMinMax.eAbsMin -= 1
        eEdictMinMax.eAbsMax += 1
        -- link to PVS leafs
        eNumClusters .= 0
        eAreaNum .= 0
        eAreaNum2 .= 0

      io (putStrLn "SVWorld.linkEdict") >> undefined -- TODO

areaEdicts :: V3 Float -> V3 Float -> V.Vector EdictT -> Int -> Int -> Quake Int
areaEdicts _ _ _ _ _ = io (putStrLn "SVWorld.areaEdicts") >> undefined -- TODO

trace :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> Quake TraceT
trace _ _ _ _ _ _ = io (putStrLn "SVWorld.trace") >> undefined -- TODO

clearWorld :: Quake ()
clearWorld = do
    initNodes

    svGlobals.svNumAreaNodes .= 0

    Just (CModelReference modelIdx) <- preuse $ svGlobals.svServer.sModels.ix 1
    Just model <- preuse $ cmGlobals.cmMapCModels.ix modelIdx

    void $ createAreaNode 0 (model^.cmMins) (model^.cmMaxs)
