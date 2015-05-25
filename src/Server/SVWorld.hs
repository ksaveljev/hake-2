{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Server.SVWorld where

import Control.Lens ((.=), (^.), (-=), (+=), use, preuse, ix, set, zoom, _1, _2, Lens', (&), (.~))
import Control.Monad (void, unless, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear.V3 (V3(..), _x, _y, _z)
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import Server.MoveClipT
import qualified Constants
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified Util.Math3D as Math3D

import qualified Debug.Trace as DT

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
            eAbsMin .= fmap (m `subtract`) (edict^.eEntityState.esOrigin)
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
                      _ -> DT.trace "HOLYMOLY" $ undefined -- should never be here

          if | (node^.anAxis) == -1 -> return node
             | (edict^.eEdictMinMax.eAbsMin.v3f) > (node^.anDist) ->
                 findCrossingNode edict (fromJust (node^.anChildren._1))
             | (edict^.eEdictMinMax.eAbsMax.v3f) < (node^.anDist) ->
                 findCrossingNode edict (fromJust (node^.anChildren._2))
             | otherwise -> return node -- crosses the node

areaEdictsR :: Int -> Quake () -- Int is reference to svGlobals.svAreaNodes
areaEdictsR nodeIdx = do
    areaType <- use $ svGlobals.svAreaType
    Just node <- preuse $ svGlobals.svAreaNodes.ix nodeIdx

    -- touch linked edicts
    let LinkReference linkIdx = if areaType == Constants.areaSolid
                                  then node^.anSolidEdicts
                                  else node^.anTriggerEdicts

    Just link <- preuse $ svGlobals.svLinks.ix linkIdx
    let linkNextRef = fromJust $ link^.lNext

    findTouching linkIdx linkNextRef

    -- if not terminal node
    unless ((node^.anAxis) == -1) $ do
      -- recurse down both sides
      areaMaxs <- use $ svGlobals.svAreaMaxs
      areaMins <- use $ svGlobals.svAreaMins

      when ((areaMaxs^.(Math3D.v3Access (node^.anAxis))) > node^.anDist) $
        areaEdictsR (fromJust $ node^.anChildren._1)

      when ((areaMins^.(Math3D.v3Access (node^.anAxis))) < node^.anDist) $
        areaEdictsR (fromJust $ node^.anChildren._2)

  where findTouching :: Int -> LinkReference -> Quake ()
        findTouching startIdx (LinkReference linkIdx)
          | startIdx == linkIdx = return ()
          | otherwise = do
              Just link <- preuse $ svGlobals.svLinks.ix linkIdx
              let Just (EdictReference edictIdx) = link^.lEdict
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
              areaMaxs <- use $ svGlobals.svAreaMaxs
              areaMins <- use $ svGlobals.svAreaMins
              areaCount <- use $ svGlobals.svAreaCount
              areaMaxCount <- use $ svGlobals.svAreaMaxCount

              if | (edict^.eSolid) == Constants.solidNot -> findTouching startIdx (fromJust $ link^.lNext)
                 | notTouching edict areaMins areaMaxs -> findTouching startIdx (fromJust $ link^.lNext)
                 | areaCount == areaMaxCount -> do
                     Com.printf "SV_areaEdicts: MAXCOUNT\n"
                     return ()
                 | otherwise -> do
                     zoom (svGlobals) $ do
                       svAreaList.ix areaCount .= EdictReference edictIdx
                       svAreaCount += 1

                     findTouching startIdx (fromJust $ link^.lNext)

        notTouching :: EdictT -> V3 Float -> V3 Float -> Bool
        notTouching edict mins maxs =
          let absmin = edict^.eEdictMinMax.eAbsMin
              absmax = edict^.eEdictMinMax.eAbsMax
          in if (absmin^._x) > (maxs^._x) ||
                (absmin^._y) > (maxs^._y) ||
                (absmin^._z) > (maxs^._z) ||
                (absmax^._x) < (mins^._x) ||
                (absmax^._y) < (mins^._y) ||
                (absmax^._z) < (mins^._z)
                then True
                else False

areaEdicts :: V3 Float -> V3 Float -> Lens' QuakeState (V.Vector EdictReference) -> Int -> Int -> Quake Int
areaEdicts mins maxs listLens maxCount areaType = do
    list <- use listLens

    zoom (svGlobals) $ do
      svAreaMins .= mins
      svAreaMaxs .= maxs
      svAreaList .= list
      svAreaCount .= 0
      svAreaMaxCount .= maxCount
      svAreaType .= areaType

    areaEdictsR 0

    updatedList <- use $ svGlobals.svAreaList
    listLens .= updatedList

    areaCount <- use $ svGlobals.svAreaCount
    return areaCount

{-
- ================== SV_Trace
- 
- Moves the given mins/maxs volume through the world from start to end.
- 
- Passedict and edicts owned by passedict are explicitly not checked.
- 
- ==================
-}
trace :: V3 Float -> Maybe (V3 Float) -> Maybe (V3 Float) -> V3 Float -> EdictReference -> Int -> Quake TraceT
trace start maybeMins maybeMaxs end passEdict contentMask = do
    vec3origin <- use $ globals.vec3Origin

    let clip = newMoveClipT

        mins = if isNothing maybeMins
                 then vec3origin
                 else fromJust maybeMins

        maxs = if isNothing maybeMaxs
                 then vec3origin
                 else fromJust maybeMaxs

    -- clip to world
    boxTraceT <- CM.boxTrace start end mins maxs 0 contentMask

    if (boxTraceT^.tFraction) == 0 -- blocked by the world
      then return $ boxTraceT { _tEnt = Just (EdictReference 0) }
      else do
        let updatedClip = clip { _mcTrace = boxTraceT { _tEnt = Just (EdictReference 0) }
                               , _mcContentMask = contentMask
                               , _mcStart = start
                               , _mcEnd = end
                               , _mcMins = mins
                               , _mcMaxs = maxs
                               , _mcPassEdict = Just passEdict
                               , _mcMins2 = mins
                               , _mcMaxs2 = maxs
                               }

        -- create the bounding box of the entire move
        let (boxMins, boxMaxs) = traceBounds start mins maxs end

        -- clip to other solid entities
        finalClip <- clipMoveToEntities $ updatedClip { _mcBoxMins = boxMins, _mcBoxMaxs = boxMaxs }

        return (finalClip^.mcTrace)


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

traceBounds :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> (V3 Float, V3 Float)
traceBounds start mins maxs end =
    let minsa = if (end^._x) > (start^._x) then (start^._x) + (mins^._x) - 1 else (end^._x) + (mins^._x) - 1
        minsb = if (end^._y) > (start^._y) then (start^._y) + (mins^._y) - 1 else (end^._y) + (mins^._y) - 1
        minsc = if (end^._z) > (start^._z) then (start^._z) + (mins^._z) - 1 else (end^._z) + (mins^._z) - 1
        maxsa = if (end^._x) > (start^._x) then (end^._x) + (maxs^._x) + 1 else (start^._x) + (maxs^._x) + 1
        maxsb = if (end^._y) > (start^._y) then (end^._y) + (maxs^._y) + 1 else (start^._y) + (maxs^._y) + 1
        maxsc = if (end^._z) > (start^._z) then (end^._z) + (maxs^._z) + 1 else (start^._z) + (maxs^._z) + 1
    in (V3 minsa minsb minsc, V3 maxsa maxsb maxsc)

clipMoveToEntities :: MoveClipT -> Quake MoveClipT
clipMoveToEntities initialClip = do
    num <- areaEdicts (initialClip^.mcBoxMins) (initialClip^.mcBoxMaxs) (svGlobals.svTouchList) Constants.maxEdicts Constants.areaSolid

    -- be careful, it is possible to have an entity in this
    -- list removed before we get to it (killtriggered)
    tryClipping 0 num initialClip

  where tryClipping :: Int -> Int -> MoveClipT -> Quake MoveClipT
        tryClipping idx maxIdx clip
          | idx >= maxIdx = return clip
          | otherwise = do
              Just touchRef@(EdictReference touchIdx) <- preuse $ svGlobals.svTouchList.ix idx
              Just touchEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix touchIdx

              (done, skip) <- shouldSkip touchRef touchEdict clip

              if done
                then
                  return clip
                else do
                  if skip
                    then tryClipping (idx + 1) maxIdx clip
                    else do
                      -- might intersect, so do an exact clip
                      headNode <- hullForEntity touchEdict
                      v3o <- use $ globals.vec3Origin
                      let angles = if (touchEdict^.eSolid) /= Constants.solidBsp
                                     then v3o -- boxes don't rotate
                                     else touchEdict^.eEntityState.esAngles
                      traceT <- if (touchEdict^.eSvFlags) .&. Constants.svfMonster /= 0
                                  then CM.transformedBoxTrace (clip^.mcStart)
                                                              (clip^.mcEnd)
                                                              (clip^.mcMins2)
                                                              (clip^.mcMaxs2)
                                                              headNode
                                                              (clip^.mcContentMask)
                                                              (touchEdict^.eEntityState.esOrigin)
                                                              angles
                                  else CM.transformedBoxTrace (clip^.mcStart)
                                                              (clip^.mcEnd)
                                                              (clip^.mcMins)
                                                              (clip^.mcMaxs)
                                                              headNode
                                                              (clip^.mcContentMask)
                                                              (touchEdict^.eEntityState.esOrigin)
                                                              angles

                      let clip' = if | (traceT^.tAllSolid) || (traceT^.tStartSolid) || (traceT^.tFraction) < clip^.mcTrace.tFraction ->
                                         let traceT' = traceT { _tEnt = Just touchRef }
                                         in if clip^.mcTrace.tStartSolid
                                              then clip { _mcTrace = traceT' { _tStartSolid = True } }
                                              else clip { _mcTrace = traceT' }
                                     | traceT^.tStartSolid ->
                                         clip & (mcTrace.tStartSolid) .~ True
                                     | otherwise ->
                                         clip

                      tryClipping (idx + 1) maxIdx clip'

        shouldSkip :: EdictReference -> EdictT -> MoveClipT -> Quake (Bool, Bool)
        shouldSkip touchRef touchEdict clip =
          if | (touchEdict^.eSolid) == Constants.solidNot -> return (False, True)
             | (Just touchRef) == (clip^.mcPassEdict) -> return (False, True)
             | clip^.mcTrace.tAllSolid -> return (True, False)
             | ((clip^.mcContentMask) .&. Constants.contentsDeadMonster == 0) && ((touchEdict^.eSvFlags) .&. Constants.svfDeadMonster /= 0) -> return (False, True)
             | isJust (clip^.mcPassEdict) -> do
                 if (touchEdict^.eOwner) == (clip^.mcPassEdict)
                   then return (False, True) -- don't clip against own missiles
                   else do
                     let Just (EdictReference passEdictIdx) = clip^.mcPassEdict
                     Just passEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix passEdictIdx
                     if passEdict^.eOwner == (Just touchRef)
                       then return (False, True) -- don't clip against owner
                       else return (False, False)
             | otherwise -> return (False, False)

{-
- ================ SV_HullForEntity
- 
- Returns a headnode that can be used for testing or clipping an object of
- mins/maxs size. Offset is filled in to contain the adjustment that must
- be added to the testing object's origin to get a point to use with the
- returned hull. ================
-}
hullForEntity :: EdictT -> Quake Int
hullForEntity edict = do
    -- decide which clipping hull to use, based on the size
    if (edict^.eSolid) == Constants.solidBsp
      then do
        -- explicit hulls in the BSP model
        Just (CModelReference modelIdx) <- preuse $ svGlobals.svServer.sModels.ix (edict^.eEntityState.esModelIndex)
        when (modelIdx == -1) $
          Com.comError Constants.errFatal "MOVETYPE_PUSH with a non bsp model"
        Just model <- preuse $ cmGlobals.cmMapCModels.ix modelIdx
        return (model^.cmHeadNode)
      -- create a temp hull from bounding box sizes
      else CM.headnodeForBox (edict^.eEdictMinMax.eMins) (edict^.eEdictMinMax.eMaxs)

pointContents :: V3 Float -> Quake Int
pointContents p = do
    -- get base contents from world
    Just (CModelReference cModelIdx) <- preuse $ svGlobals.svServer.sModels.ix 1
    Just headNode <- preuse $ cmGlobals.cmMapCModels.ix cModelIdx.cmHeadNode
    contents <- CM.pointContents p headNode
    -- or in contents from all the other entities
    num <- areaEdicts p p (svGlobals.svTouch) Constants.maxEdicts Constants.areaSolid

    collectContents contents 0 num

  where collectContents :: Int -> Int -> Int -> Quake Int
        collectContents contents idx maxIdx
          | idx >= maxIdx = return contents
          | otherwise = do
              Just (EdictReference edictIdx) <- preuse $ svGlobals.svTouch.ix idx
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
              -- might intersect, so do an exact clip
              headNode <- hullForEntity edict
              when ((edict^.eSolid) /= Constants.solidBsp) $
                return () -- TODO: find out why this is here
              c2 <- CM.transformedPointContents p headNode (edict^.eEntityState.esOrigin) (edict^.eEntityState.esAngles)
              collectContents (contents .|. c2) (idx + 1) maxIdx
