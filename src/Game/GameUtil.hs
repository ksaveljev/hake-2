{-# LANGUAGE OverloadedStrings #-}
module Game.GameUtil where

import Control.Lens ((^.), use, (.=), ix, preuse, (+=))
import Control.Monad (liftM, when)
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Constants

{-
- Either finds a free edict, or allocates a new one. Try to avoid reusing
- an entity that was recently freed, because it can cause the client to
- think the entity morphed into something else instead of being removed and
- recreated, which can cause interpolated angles and bad trails.
-}
spawn :: Quake EdictReference
spawn = do
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
    numEdicts <- use $ gameBaseGlobals.gbNumEdicts
    edicts <- use $ gameBaseGlobals.gbGEdicts
    time <- use $ gameBaseGlobals.gbLevel.llTime

    let foundIndex = findFreeEdict edicts (maxClientsValue+1) (numEdicts-1) time

    case foundIndex of
      Just er@(EdictReference idx) -> do
        gameBaseGlobals.gbGEdicts.ix idx .= newEdictT idx
        initEdict er
        return er
      Nothing -> do
        maxEntities <- use $ gameBaseGlobals.gbGame.glMaxEntities

        when (numEdicts == maxEntities) $ do
          err <- use $ gameBaseGlobals.gbGameImport.giError
          err "ED_Alloc: no free edicts"

        gameBaseGlobals.gbGEdicts.ix numEdicts .= newEdictT numEdicts
        gameBaseGlobals.gbNumEdicts += 1
        initEdict (EdictReference numEdicts)
        return (EdictReference numEdicts)

  where findFreeEdict :: V.Vector EdictT -> Int -> Int -> Float -> Maybe EdictReference
        findFreeEdict edicts idx maxIdx levelTime
          | idx == maxIdx = Nothing
          | otherwise = let e = edicts V.! idx
                            notInUse = not (e^.eInUse)
                            freeTime = e^.eFreeTime
                        in if notInUse && (freeTime < 2 || levelTime - freeTime > 0.5)
                             then Just (EdictReference idx)
                             else findFreeEdict edicts (idx + 1) maxIdx levelTime

initEdict :: EdictReference -> Quake ()
initEdict er@(EdictReference idx) = do
    Just e <- preuse $ gameBaseGlobals.gbGEdicts.ix idx

    let updatedEdict = e { _eInUse        = True
                         , _eClassName    = "noclass"
                         , _eEdictPhysics = (e^.eEdictPhysics) { _eGravity = 1.0 }
                         , _eEntityState  = (newEntityStateT (Just er)) { _esNumber = idx }
                         , _eIndex        = idx
                         }

    gameBaseGlobals.gbGEdicts.ix idx .= updatedEdict

{-
- Call after linking a new trigger in during gameplay to force all entities
- it covers to immediately touch it.
-}
clearEdict :: EdictReference -> Quake ()
clearEdict (EdictReference idx) = gameBaseGlobals.gbGEdicts.ix idx .= newEdictT idx

-- Marks the edict as free
freeEdict :: EdictReference -> Quake ()
freeEdict er@(EdictReference idx) = do
    unlinkEntity <- use $ gameBaseGlobals.gbGameImport.giUnlinkEntity
    unlinkEntity er

    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

    when (idx > maxClientsValue + Constants.bodyQueueSize) $ do
      time <- use $ gameBaseGlobals.gbLevel.llTime
      gameBaseGlobals.gbGEdicts.ix idx .= (newEdictT idx) { _eClassName = "freed", _eFreeTime = time, _eInUse = False }
