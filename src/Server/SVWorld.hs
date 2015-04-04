{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.SVWorld where

import Control.Lens ((.=), (^.), (-=), (+=), use, preuse, ix, set, zoom, _1, _2)
import Control.Monad (void, unless, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear.V3 (V3, _x, _y, _z)
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com

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
      solid <- if | (edict^.eSolid) == Constants.solidBbox && 0 == ((edict^.eSvFlags) .&. Constants.svfDeadMonster) -> do
                          -- assume that x/y are equal and symetric
                      let i :: Int = truncate ((edict^.eEdictMinMax.eMaxs._x) / 8)
                          -- z is not symetric
                          j :: Int = truncate ((edict^.eEdictMinMax.eMins._z) / (-8))
                          -- and z maxs can be negative
                          k :: Int= truncate ((32 + (edict^.eEdictMinMax.eMaxs._z)) / 8)

                          i' = if | i < 1 -> 1
                                  | i > 31 -> 31
                                  | otherwise -> i

                          j' = if | j < 1 -> 1
                                  | j > 31 -> 31
                                  | otherwise -> j

                          k' = if | k < 1 -> 1
                                  | k > 63 -> 63
                                  | otherwise -> k

                      return $ (k' `shiftL` 10) .|. (j' `shiftL` 5) .|. i'
                  | (edict^.eSolid) == Constants.solidBsp ->
                      -- a solid _bbox will never create this value
                      return 31
                  | otherwise ->
                      return  0

      gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSolid .= solid

      -- set the abs box
      if solid == Constants.solidBsp && F.any (/= 0) (edict^.eEntityState.esAngles)
        then do
          -- expand for rotation
          let aMins = fmap abs (edict^.eEdictMinMax.eMins)
              aMaxs = fmap abs (edict^.eEdictMinMax.eMaxs)
              minMax = F.maximum aMins
              maxMax = F.maximum aMaxs
              m = F.maximum [minMax, maxMax]

          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictMinMax) $ do
            eAbsMin .= fmap (`subtract` m) (edict^.eEntityState.esOrigin)
            eAbsMax .= fmap (+ m) (edict^.eEntityState.esOrigin)

        else
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

      Just updatedEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
      (numLeafs, iw) <- CM.boxLeafNums (updatedEdict^.eEdictMinMax.eAbsMin)
                                      (updatedEdict^.eEdictMinMax.eAbsMax)
                                      (svGlobals.svLeafs)
                                      128
                                      [0]

      let topnode = head iw

      -- set areas
      mapM_ setAreas [0..numLeafs-1]

      if numLeafs >= 128
        then -- assume we missed some leafs, and mark by headnode
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eNumClusters .= (-1)
            eHeadNode .= topnode
        else do
          gameBaseGlobals.gbGEdicts.ix edictIdx.eNumClusters .= 0
          setHeadNode topnode 0 numLeafs

      -- if first time, make sure old_origin is valid
      when ((updatedEdict^.eLinkCount) == 0) $
        gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOldOrigin .= (updatedEdict^.eEntityState.esOrigin)

      gameBaseGlobals.gbGEdicts.ix edictIdx.eLinkCount += 1

      unless ((updatedEdict^.eSolid) == Constants.solidNot) $ do
        -- find the first node that the ent's box crosses
        node <- findCrossingNode updatedEdict 0

        -- link it in
        if (updatedEdict^.eSolid) == Constants.solidTrigger
          then insertLinkBefore (updatedEdict^.eArea) (node^.anTriggerEdicts)
          else insertLinkBefore (updatedEdict^.eArea) (node^.anSolidEdicts)

  where setAreas :: Int -> Quake ()
        setAreas idx = do
          leafCluster <- CM.leafCluster idx
          svGlobals.svClusters.ix idx .= leafCluster

          area <- CM.leafArea idx
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          when (area /= 0) $
            -- doors may legally straggle two areas,
            -- but nothing should ever need more than that
            if (edict^.eAreaNum) /= 0 && (edict^.eAreaNum) /= area
              then do
                state <- use $ svGlobals.svServer.sState

                when ((edict^.eAreaNum2) /= 0 && (edict^.eAreaNum2) /= area && state == Constants.ssLoading) $
                  Com.dprintf $ "Object touching 3 areas at " `B.append` BC.pack (show (edict^.eEdictMinMax.eAbsMin)) `B.append` "\n"

                gameBaseGlobals.gbGEdicts.ix edictIdx.eAreaNum2 .= area
              else 
                gameBaseGlobals.gbGEdicts.ix edictIdx.eAreaNum .= area

        setHeadNode :: Int -> Int -> Int -> Quake ()
        setHeadNode topnode idx maxIdx
          | idx == maxIdx = return ()
          | otherwise = do
              clusters <- use $ svGlobals.svClusters
              let c = clusters UV.! idx

              if c == -1 -- not a visible leaf
                then setHeadNode topnode (idx + 1) maxIdx
                else do
                  let foundIndex = UV.findIndex (\v -> v == c) (UV.take idx clusters)

                  if isNothing foundIndex
                    then do
                      Just numClusters <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eNumClusters

                      if numClusters == Constants.maxEntClusters
                        then do
                          -- assume we missed some leafs, and mark by headnode
                          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
                            eNumClusters .= (-1)
                            eHeadNode .= topnode
                        else do
                          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
                            eClusterNums.ix numClusters .= c
                            eNumClusters += 1

                          setHeadNode topnode (idx + 1) maxIdx

                    else setHeadNode topnode (idx + 1) maxIdx

        findCrossingNode :: EdictT -> Int -> Quake AreaNodeT
        findCrossingNode edict idx = do
          Just node <- preuse $ svGlobals.svAreaNodes.ix idx

          let v3f = case (node^.anAxis) of
                      0 -> _x
                      1 -> _y
                      2 -> _z
                      _ -> undefined -- should never be here

          if | (node^.anAxis) == -1 -> return node
             | (edict^.eEdictMinMax.eAbsMin.v3f) > (node^.anDist) ->
                 findCrossingNode edict (fromJust (node^.anChildren._1))
             | (edict^.eEdictMinMax.eAbsMax.v3f) < (node^.anDist) ->
                 findCrossingNode edict (fromJust (node^.anChildren._2))
             | otherwise -> return node -- crosses the node

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

insertLinkBefore :: LinkReference -> LinkReference -> Quake ()
insertLinkBefore v@(LinkReference vIdx) before@(LinkReference beforeIdx) = do
    Just beforePrev <- preuse $ svGlobals.svLinks.ix beforeIdx.lPrev
    svGlobals.svLinks.ix vIdx.lNext .= Just before
    svGlobals.svLinks.ix vIdx.lPrev .= beforePrev
    let Just (LinkReference beforePrevIdx) = beforePrev
    svGlobals.svLinks.ix beforePrevIdx.lNext .= Just v
    svGlobals.svLinks.ix beforeIdx.lPrev .= Just v
