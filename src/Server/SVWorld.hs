{-# LANGUAGE Rank2Types #-}
module Server.SVWorld
    ( areaEdicts
    , clearWorld
    , linkEdict
    , pointContents
    , trace
    , unlinkEdict
    ) where

import           Control.Lens        (Lens', use, preuse, ix, (^.), (.=), (+=), (&), (.~), (-~), (+~), _1, _2)
import           Control.Monad       (void, when, unless)
import           Data.Bits           (shiftL, (.&.), (.|.))
import qualified Data.ByteString     as B
import qualified Data.Foldable       as F
import           Data.Maybe          (isJust)
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV
import           Linear              (V3(..), _x, _y, _z)

import qualified Constants
import           Game.CModelT
import           Game.EdictT
import           Game.EntityStateT
import           Game.LinkT
import qualified QCommon.CM          as CM
import qualified QCommon.Com         as Com
import           QuakeRef
import           QuakeState
import           Server.AreaNodeT
import           Server.ServerT
import           Types
import           Util.Binary         (encode)

areaEdicts :: V3 Float -> V3 Float -> Lens' QuakeState (V.Vector (Ref EdictT)) -> Int -> Int -> Quake Int
areaEdicts = error "SVWorld.areaEdicts" -- TODO

clearWorld :: Quake ()
clearWorld = do
    initNodes
    svGlobals.svNumAreaNodes .= 0
    modelRef <- preuse (svGlobals.svServer.sModels.ix 1)
    maybe modelRefError proceedClear modelRef
  where
    modelRefError = error "SVWorld.clearWorld modelRef is Nothing"
    proceedClear modelRef = do
        model <- readRef modelRef
        void (createAreaNode 0 (model^.cmMins) (model^.cmMaxs))

linkEdict :: Ref EdictT -> Quake ()
linkEdict edictRef = do
    edict <- readRef edictRef
    link <- readRef (edict^.eArea)
    when (isJust (link^.lPrev)) $
        unlinkEdict edictRef
    -- don't add the world and the one not in use
    unless ((edict^.eIndex) == 0 || not (edict^.eInUse)) $ do
        -- set the size
        modifyRef edictRef (\v -> v & eSize .~ (edict^.eMaxs) - (edict^.eMins))
        -- encode the size into the entity_state for client prediction
        let solid = calcSolid edict
        modifyRef edictRef (\v -> v & eEntityState.esSolid .~ solid)
        setAbsBox edict solid
        -- because movement is clipped an epsilon away from an actual edge,
        -- we must fully check even when bouding boxes don't quite touch
        modifyRef edictRef (\v -> v & eAbsMin -~ 1
                                    & eAbsMax +~ 1
                                      -- link to PVS leafs
                                    & eNumClusters .~ 0
                                    & eAreaNum .~ 0
                                    & eAreaNum2 .~ 0)
        updatedEdict <- readRef edictRef
        (numLeafs, Just iw) <- CM.boxLeafNums (updatedEdict^.eAbsMin)
                                              (updatedEdict^.eAbsMax)
                                              (svGlobals.svLeafs)
                                              128
                                              (Just [0])
        let topnode = head iw -- IMPROVE: so bad, [Just iw] above should also be thought about
        mapM_ setAreas [0..numLeafs-1]
        markHeadNode numLeafs topnode
        -- if first time, make sure old_origin is valid
        when ((updatedEdict^.eLinkCount) == 0) $
            modifyRef edictRef (\v -> v & eEntityState.esOldOrigin .~ (updatedEdict^.eEntityState.esOrigin))
        modifyRef edictRef (\v -> v & eLinkCount +~ 1)
        unless ((updatedEdict^.eSolid) == Constants.solidNot) $ do
            -- find the first node that the ent's box crosses
            node <- findCrossingNode updatedEdict (Just (Ref 0))
            -- link it in
            let linkRef | (updatedEdict^.eSolid) == Constants.solidTrigger = node^.anTriggerEdicts
                        | otherwise                                        = node^.anSolidEdicts
            insertLinkBefore (updatedEdict^.eArea) linkRef
  where
    calcSolid edict
        | (edict^.eSolid) == Constants.solidBbox && 0 == ((edict^.eSvFlags) .&. Constants.svfDeadMonster) =
                -- assume that x/y are equal and symetric
            let i = truncate ((edict^.eMaxs._x) / 8) :: Int
                -- z is not symetric
                j = truncate ((edict^.eMins._z) / (-8)) :: Int
                -- and z maxs can be negative
                k = truncate ((32 + (edict^.eMaxs._z)) / 8) :: Int
                i' | i < 1 = 1
                   | i > 31 = 31
                   | otherwise = i
                j' | j < 1 = 1
                   | j > 31 = 31
                   | otherwise = j
                k' | k < 1 = 1
                   | k > 63 = 63
                   | otherwise = k
            in (k' `shiftL` 10) .|. (j' `shiftL` 5) .|. i'
        | (edict^.eSolid) == Constants.solidBsp = 31 -- a solid _bbox will never create this value
        | otherwise = 0
    setAbsBox edict solid
        | solid == Constants.solidBsp && F.any (/= 0) (edict^.eEntityState.esAngles) = do
            -- expand for rotation
            let aMins = fmap abs (edict^.eMins)
                aMaxs = fmap abs (edict^.eMaxs)
                minMax = F.maximum aMins
                maxMax = F.maximum aMaxs
                m = F.maximum [minMax, maxMax]
            modifyRef edictRef (\v -> v & eAbsMin .~ fmap (m `subtract`) (edict^.eEntityState.esOrigin)
                                        & eAbsMax .~ fmap (+ m) (edict^.eEntityState.esOrigin))
        | otherwise =
            -- normal
            modifyRef edictRef (\v -> v & eAbsMin .~ (edict^.eEntityState.esOrigin) + (edict^.eMins)
                                        & eAbsMax .~ (edict^.eEntityState.esOrigin) + (edict^.eMaxs))
    setAreas idx = do
          leafs <- use (svGlobals.svLeafs)
          leafCluster <- CM.leafCluster (leafs UV.! idx)
          svGlobals.svClusters.ix idx .= leafCluster
          area <- CM.leafArea (leafs UV.! idx)
          edict <- readRef edictRef
          when (area /= 0) $
              setAreaNum edict area
    -- doors may legally straggle two areas, but nothing should ever need more than that
    setAreaNum edict area
        | (edict^.eAreaNum) /= 0 && (edict^.eAreaNum) /= area = do
            state <- use (svGlobals.svServer.sState)
            when ((edict^.eAreaNum2) /= 0 && (edict^.eAreaNum2) /= area && state == Constants.ssLoading) $
                Com.dprintf (B.concat ["Object touching 3 areas at ", encode (edict^.eAbsMin), "\n"])
            modifyRef edictRef (\v -> v & eAreaNum2 .~ area)
        | otherwise =
            modifyRef edictRef (\v -> v & eAreaNum .~ area)
    markHeadNode numLeafs topnode
        | numLeafs >= 128 =
            -- assume we missed some leafs, and mark by headnode
            modifyRef edictRef (\v -> v & eNumClusters .~ (-1)
                                        & eHeadNode .~ topnode)
        | otherwise = do
            modifyRef edictRef (\v -> v & eNumClusters .~ 0)
            setHeadNode edictRef topnode 0 numLeafs
    findCrossingNode _ Nothing = do
        Com.fatalError "SVWorld.linkEdict#findCrossingNode idx is Nothing"
        undefined
    findCrossingNode edict (Just nodeRef) = do
          node <- readRef nodeRef
          let v3f = case (node^.anAxis) of
                      0 -> _x
                      1 -> _y
                      2 -> _z
                      _ -> error "SVWorld.linkEdict should never be here"
          case () of _
                       | (node^.anAxis) == -1 -> return node
                       | (edict^.eAbsMin.v3f) > (node^.anDist) ->
                           findCrossingNode edict (node^.anChildren._1)
                       | (edict^.eAbsMax.v3f) < (node^.anDist) ->
                           findCrossingNode edict (node^.anChildren._2)
                       | otherwise -> return node -- crosses the node

setHeadNode :: Ref EdictT -> Int -> Int -> Int -> Quake ()
setHeadNode edictRef topnode idx maxIdx
    | idx == maxIdx = return ()
    | otherwise = do
        clusters <- use (svGlobals.svClusters)
        doSetHeadNode clusters (clusters UV.! idx)
  where
    doSetHeadNode clusters c
        | c == -1 = nextNode
        | otherwise = do
            let foundIndex = UV.findIndex (\v -> v == c) (UV.take idx clusters)
            maybe (searchForNode c) (\_ -> nextNode) foundIndex
    nextNode = setHeadNode edictRef topnode (idx + 1) maxIdx
    searchForNode c = do
        edict <- readRef edictRef
        doSearchForNode c (edict^.eNumClusters)
    doSearchForNode c numClusters
        | numClusters == Constants.maxEntClusters = do
            -- assume we missed some leafs, and mark by headnode
            modifyRef edictRef (\v -> v & eNumClusters .~ (-1)
                                        & eHeadNode .~ topnode)
        | otherwise = do
            modifyRef edictRef (\v -> v & eClusterNums.ix numClusters .~ c
                                        & eNumClusters +~ 1)
            nextNode

pointContents :: V3 Float -> Quake Int
pointContents = error "SVWorld.pointContents" -- TODO

trace :: V3 Float -> Maybe (V3 Float) -> Maybe (V3 Float) -> V3 Float -> Maybe (Ref EdictT) -> Int -> Quake TraceT
trace = error "SVWorld.trace" -- TODO

unlinkEdict :: Ref EdictT -> Quake ()
unlinkEdict edictRef = do
    edict <- readRef edictRef
    link <- readRef (edict^.eArea)
    when (isJust (link^.lPrev)) $ do
        removeLink (edict^.eArea)
        modifyRef (edict^.eArea) (\v -> v & lNext .~ Nothing
                                          & lPrev .~ Nothing)

removeLink :: Ref LinkT -> Quake ()
removeLink linkRef = do
    link <- readRef linkRef
    doRemoveLink link (link^.lNext) (link^.lPrev)
  where
    doRemoveLink _ Nothing _ = Com.fatalError "SVWorld.removeLink lNext is Nothing"
    doRemoveLink _ _ Nothing = Com.fatalError "SVWorld.removeLink lPrev is Nothing"
    doRemoveLink link (Just nextRef) (Just prevRef) = do
        modifyRef nextRef (\v -> v & lPrev .~ (link^.lPrev))
        modifyRef prevRef (\v -> v & lNext .~ (link^.lNext))

initNodes :: Quake ()
initNodes = svGlobals.svAreaNodes .= V.generate Constants.areaNodes newAreaNodeT

createAreaNode :: Int -> V3 Float -> V3 Float -> Quake (Ref AreaNodeT)
createAreaNode depth mins maxs = do
    numAreaNodes <- use (svGlobals.svNumAreaNodes)
    svGlobals.svNumAreaNodes += 1
    areaNode <- readRef (Ref numAreaNodes)
    clearLink (areaNode^.anTriggerEdicts)
    clearLink (areaNode^.anSolidEdicts)
    clearNode numAreaNodes
    return (Ref numAreaNodes)
  where
    clearNode numAreaNodes
        | depth == Constants.areaDepth =
            modifyRef (Ref numAreaNodes) (\v -> v & anAxis .~ (-1)
                                                  & anChildren .~ (Nothing, Nothing))
        | otherwise = createChildrenAreaNodes (Ref numAreaNodes) depth mins maxs

createChildrenAreaNodes :: Ref AreaNodeT -> Int -> V3 Float -> V3 Float -> Quake ()
createChildrenAreaNodes areaNodeRef depth mins maxs = do
    child1 <- createAreaNode (depth + 1) mins2 maxs2
    child2 <- createAreaNode (depth + 1) mins1 maxs1
    modifyRef areaNodeRef (\v -> v & anAxis .~ axis
                                   & anDist .~ dist
                                   & anChildren .~ (Just child1, Just child2))
  where
    size = maxs - mins
    axis = if (size^._x) > (size^._y) then 0 else 1 :: Int
    dist = 0.5 * ((if axis == 0 then maxs^._x else maxs^._y) + (if axis == 0 then mins^._x else mins^._y))
    mins1 = mins
    mins2 = if axis == 0 then mins & _x .~ dist else mins & _y .~ dist
    maxs1 = if axis == 0 then maxs & _x .~ dist else maxs & _y .~ dist
    maxs2 = maxs

clearLink :: Ref LinkT -> Quake ()
clearLink linkRef = modifyRef linkRef (\v -> v & lNext .~ Just linkRef
                                               & lPrev .~ Just linkRef)

insertLinkBefore :: Ref LinkT -> Ref LinkT -> Quake ()
insertLinkBefore linkRef beforeRef = do
    before <- readRef beforeRef
    modifyRef linkRef (\v -> v & lNext .~ Just beforeRef)
    modifyRef linkRef (\v -> v & lPrev .~ (before^.lPrev))
    maybe linkError setBeforeNext (before^.lPrev)
    modifyRef beforeRef (\v -> v & lPrev .~ Just linkRef)
  where
    linkError = Com.fatalError "SVWorld.insertLinkBefore before^.lPrev is Nothing"
    setBeforeNext prevRef = modifyRef prevRef (\v -> v & lNext .~ Just linkRef)
