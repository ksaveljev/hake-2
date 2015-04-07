{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameUtil where

import Control.Lens ((^.), use, (.=), ix, preuse, (+=), zoom)
import Control.Monad (liftM, when, unless)
import Data.Bits ((.&.))
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (norm)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameBase as GameBase

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

{-
- Returns the range catagorization of an entity reletive to self 0 melee
- range, will become hostile even if back is turned 1 visibility and
- infront, or visibility and show hostile 2 infront and show hostile 3 only
- triggered by damage.
-}
range :: EdictT -> EdictT -> Int
range self other =
    let v = (self^.eEntityState.esOrigin) - (other^.eEntityState.esOrigin)
        len = norm v
    in if | len < (fromIntegral Constants.meleeDistance) -> Constants.rangeMelee
          | len < 500 -> Constants.rangeNear
          | len < 1000 -> Constants.rangeMid
          | otherwise -> Constants.rangeFar

{-
- Use the targets.
- 
- The global "activator" should be set to the entity that initiated the
- firing.
- 
- If self.delay is set, a DelayedUse entity will be created that will
- actually do the SUB_UseTargets after that many seconds have passed.
- 
- Centerprints any self.message to the activator.
- 
- Search for (string)targetname in all entities that match
- (string)self.target and call their .use function
-}
useTargets :: EdictReference -> Maybe EdictReference -> Quake ()
useTargets (EdictReference edictIdx) activatorReference = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let dprintf = gameImport^.giDprintf
        sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex
        centerPrintf = gameImport^.giCenterPrintf
    
    -- check for a delay
    if (edict^.eDelay) /= 0
      then do
        -- create a temp object to fire at a later time
        EdictReference tmpIdx <- spawn
        time <- use $ gameBaseGlobals.gbLevel.llTime

        when (isNothing activatorReference) $ do
          dprintf "Think_Delay with no activator\n"

        zoom (gameBaseGlobals.gbGEdicts.ix tmpIdx) $ do
          eClassName .= "DelayedUse"
          eEdictAction.eaNextThink .= time + (edict^.eDelay)
          eEdictAction.eaThink .= Just thinkDelay
          eEdictOther.eoActivator .= activatorReference
          eEdictInfo.eiMessage .= (edict^.eEdictInfo.eiMessage)
          eEdictInfo.eiTarget .= (edict^.eEdictInfo.eiTarget)
          eEdictInfo.eiKillTarget .= (edict^.eEdictInfo.eiKillTarget)

      else do
        let ar@(EdictReference activatorIdx) = fromJust activatorReference
        Just activator <- preuse $ gameBaseGlobals.gbGEdicts.ix activatorIdx

        -- print the message
        when (isJust (edict^.eEdictInfo.eiMessage) && ((activator^.eSvFlags) .&. Constants.svfMonster) == 0) $ do
          centerPrintf ar (fromJust (edict^.eEdictInfo.eiMessage))
          if (edict^.eNoiseIndex) /= 0
            then sound ar Constants.chanAuto (edict^.eNoiseIndex) 1 (fromIntegral Constants.attnNorm) 0
            else do
              talkIdx <- soundIndex "misc/talk1.wav"
              sound ar Constants.chanAuto talkIdx 1 (fromIntegral Constants.attnNorm) 0

        done <- if (isJust (edict^.eEdictInfo.eiKillTarget))
                  then killKillTargets Nothing (fromJust $ edict^.eEdictInfo.eiKillTarget)
                  else return False

        unless done $ do
          -- fire targets
          io (putStrLn "GameUtil.useTargets") >> undefined -- TODO

  where killKillTargets :: Maybe EdictReference -> B.ByteString -> Quake Bool
        killKillTargets edictRef killTarget = do
          nextRef <- GameBase.gFind edictRef GameBase.findByTarget killTarget
          if isJust nextRef
            then do
              let Just newReference = nextRef
              freeEdict newReference
              Just inUse <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eInUse
              if inUse
                then killKillTargets nextRef killTarget
                else do
                  dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
                  dprintf "entity was removed while using killtargets\n"
                  return True
            else return False

thinkDelay :: EntThink
thinkDelay =
  GenericEntThink "Think_Delay" $ \er@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    useTargets er (edict^.eEdictOther.eoActivator)
    freeEdict er
    return True

freeEdictA :: EntThink
freeEdictA =
  GenericEntThink "G_FreeEdictA" $ \er -> do
    freeEdict er
    return False
