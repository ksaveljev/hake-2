{-# LANGUAGE Rank2Types #-}
module Server.SVWorld
    ( areaEdicts
    , clearWorld
    , linkEdict
    , pointContents
    , trace
    , unlinkEdict
    ) where

import           Control.Lens     (Lens', use, preuse, ix, (^.), (.=), (+=), (&), (.~))
import           Control.Monad    (void)
import           Data.Maybe       (fromMaybe)
import qualified Data.Vector      as V
import           Linear           (V3(..), _x, _y, _z)

import qualified Constants
import           Game.CModelT
import           Game.EdictT
import           Game.LinkT
import           Game.TraceT
import qualified QCommon.CM       as CM
import qualified QCommon.Com      as Com
import           QuakeRef
import           QuakeState
import           Server.AreaNodeT
import           Server.MoveClipT
import           Server.ServerT
import           Types

trace :: V3 Float -> Maybe (V3 Float) -> Maybe (V3 Float) -> V3 Float -> Maybe (Ref EdictT) -> Int -> Quake TraceT
trace start maybeMins maybeMaxs end passEdictRef contentMask = do
    (mins, maxs) <- fmap getMinsAndMaxs (use (globals.gVec3Origin))
    boxTraceT <- CM.boxTrace start end mins maxs 0 contentMask
    doTrace boxTraceT mins maxs
  where
    getMinsAndMaxs v3o = (fromMaybe v3o maybeMins, fromMaybe v3o maybeMaxs)
    doTrace boxTraceT mins maxs
        | (boxTraceT^.tFraction) == 0 = return (boxTraceT & tEnt .~ Just worldRef) -- blocked by the world
        | otherwise = do
            finalClip <- clipMoveToEntities (updatedClip & mcBoxMins .~ boxMins
                                                         & mcBoxMaxs .~ boxMaxs)
            return (finalClip^.mcTrace)
      where
        updatedClip = newMoveClipT & mcTrace .~ (boxTraceT & tEnt .~ Just worldRef)
                                   & mcContentMask .~ contentMask
                                   & mcStart .~ start
                                   & mcEnd .~ end
                                   & mcMins .~ mins
                                   & mcMaxs .~ maxs
                                   & mcPassEdict .~ passEdictRef
                                   & mcMins2 .~ mins
                                   & mcMaxs2 .~ maxs
        (boxMins, boxMaxs) = traceBounds start mins maxs end

unlinkEdict :: Ref EdictT -> Quake ()
unlinkEdict edictRef = do
    edict <- readRef edictRef
    link <- readRef (edict^.eArea)
    maybe (return ()) (doUnlinkEdict link (edict^.eArea)) (link^.lPrev)
  where
    doUnlinkEdict link linkRef _ = do
        removeLink linkRef
        writeRef linkRef (link & lNext .~ Nothing
                               & lPrev .~ Nothing)
        

linkEdict :: Ref EdictT -> Quake ()
linkEdict = error "SVWorld.linkEdict" -- TODO

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

pointContents :: V3 Float -> Quake Int
pointContents = error "SVWorld.pointContents" -- TODO

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

initNodes :: Quake ()
initNodes = svGlobals.svAreaNodes .= V.generate Constants.areaNodes newAreaNodeT

clearLink :: Ref LinkT -> Quake ()
clearLink linkRef = modifyRef linkRef (\v -> v & lNext .~ Just linkRef
                                               & lPrev .~ Just linkRef)

removeLink :: Ref LinkT -> Quake ()
removeLink linkRef = do
    link <- readRef linkRef
    unsetLink link (link^.lNext) (link^.lPrev)
  where
    unsetLink _ Nothing _ = linkError
    unsetLink _ _ Nothing = linkError
    unsetLink link (Just nextRef) (Just prevRef) = do
        modifyRef nextRef (\v -> v & lPrev .~ (link^.lPrev))
        modifyRef prevRef (\v -> v & lNext .~ (link^.lNext))
    linkError = Com.fatalError "SVWorld.removeLink link next or prev is Nothing"

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
clipMoveToEntities = error "SVWorld.clipMoveToEntities" -- TODO