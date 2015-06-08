{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameUtil where

import Control.Lens ((^.), use, (.=), ix, preuse, (+=), zoom, (%=))
import Control.Monad (liftM, when, unless, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (norm)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameCombat as GameCombat
import qualified Util.Math3D as Math3D

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

    -- search for a free edict starting from index (maxClientsValue+1) up
    -- to (numEdicts-1)
    let foundIndex = findFreeEdict (V.drop (maxClientsValue + 1) edicts) time maxClientsValue numEdicts

    case foundIndex of
      Just idx -> do
        gameBaseGlobals.gbGEdicts.ix idx .= newEdictT idx
        initEdict (EdictReference idx)
        return (EdictReference idx)
      Nothing -> do
        maxEntities <- use $ gameBaseGlobals.gbGame.glMaxEntities

        when (numEdicts == maxEntities) $ do
          err <- use $ gameBaseGlobals.gbGameImport.giError
          err "ED_Alloc: no free edicts"

        gameBaseGlobals.gbGEdicts.ix numEdicts .= newEdictT numEdicts
        gameBaseGlobals.gbNumEdicts += 1
        initEdict (EdictReference numEdicts)
        return (EdictReference numEdicts)

  where findFreeEdict :: V.Vector EdictT -> Float -> Int -> Int -> Maybe Int
        findFreeEdict edicts levelTime maxClientsValue numEdicts =
          let found = V.findIndex (\edict -> (not $ edict^.eInUse) && ((edict^.eFreeTime) < 2 || levelTime - (edict^.eFreeTime) > 0.5)) edicts
          in case found of
               Nothing -> Nothing
               Just idx -> if idx + maxClientsValue + 1 >= numEdicts
                             then Nothing
                             else Just (idx + maxClientsValue + 1)

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
useTargets er@(EdictReference edictIdx) activatorReference = do
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
            then sound (Just ar) Constants.chanAuto (edict^.eNoiseIndex) 1 Constants.attnNorm 0
            else do
              talkIdx <- soundIndex (Just "misc/talk1.wav")
              sound (Just ar) Constants.chanAuto talkIdx 1 Constants.attnNorm 0

        -- kill killtargets
        done <- if (isJust (edict^.eEdictInfo.eiKillTarget))
                  then killKillTargets Nothing (fromJust $ edict^.eEdictInfo.eiKillTarget)
                  else return False

        unless done $ do
          -- fire targets
          when (isJust $ edict^.eEdictInfo.eiTarget) $ do
            fireTargets (BC.map toLower (edict^.eClassName)) Nothing GameBase.findByTarget (fromJust $ edict^.eEdictInfo.eiTarget)

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

        fireTargets :: B.ByteString -> Maybe EdictReference -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake ()
        fireTargets edictClassName ref findBy targetName = do
          edictRef <- GameBase.gFind ref findBy targetName

          when (isJust edictRef) $ do
            let Just fr@(EdictReference foundEdictIdx) = edictRef
            Just foundEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix foundEdictIdx

            -- doors fire area portals in a specific way
            let foundEdictClassName = BC.map toLower (foundEdict^.eClassName)
            if foundEdictClassName == "func_areaportal" && (any (== edictClassName) ["func_door", "func_door_rotating"])
              then fireTargets edictClassName edictRef findBy targetName
              else do
                dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf

                if foundEdictIdx == edictIdx
                  then dprintf "WARNING: Entity used iteself.\n"
                  else
                    when (isJust $ foundEdict^.eEdictAction.eaUse) $
                      entUse (fromJust $ foundEdict^.eEdictAction.eaUse) fr (Just er) activatorReference

                Just inUse <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eInUse

                if not inUse
                  then dprintf "entity was removed while using targets\n"
                  else fireTargets edictClassName edictRef findBy targetName

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

monsterUse :: EntUse
monsterUse =
  GenericEntUse "monster_use" $ \_ _ _ -> do
    io (putStrLn "GameUtil.monsterUse") >> undefined -- TODO

mCheckAttack :: EntThink
mCheckAttack =
  GenericEntThink "M_CheckAttack" $ \_ -> do
    io (putStrLn "GameUtil.mCheckAttack") >> undefined -- TODO

{-
- Kills all entities that would touch the proposed new positioning of ent.
- Ent should be unlinked before calling this!
-}
killBox :: EdictReference -> Quake Bool
killBox edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    trace <- use $ gameBaseGlobals.gbGameImport.giTrace
    traceT <- trace (edict^.eEntityState.esOrigin)
                    (Just $ edict^.eEdictMinMax.eMins)
                    (Just $ edict^.eEdictMinMax.eMaxs)
                    (edict^.eEntityState.esOrigin)
                    Nothing
                    Constants.maskPlayerSolid

    if isNothing (traceT^.tEnt) || (traceT^.tEnt) == Just (EdictReference 0)
      then
        return True
      else do
        -- nail it
        v3o <- use $ globals.vec3Origin
        let Just traceEntRef@(EdictReference traceEntIdx) = traceT^.tEnt
        GameCombat.damage traceEntRef
                          edictRef
                          edictRef
                          v3o
                          (edict^.eEntityState.esOrigin)
                          v3o
                          100000
                          0
                          Constants.damageNoProtection
                          Constants.modTelefrag

        -- if we didnt' kill it, fail
        Just solid <- preuse $ gameBaseGlobals.gbGEdicts.ix traceEntIdx.eSolid
        if solid /= 0
          then return False
          else killBox edictRef

visible :: EdictReference -> EdictReference -> Quake Bool
visible _ _ = do
    io (putStrLn "GameUtil.visible") >> undefined -- TODO

{-
- Finds a target.
-
- Self is currently not attacking anything, so try to find a target
-
- Returns TRUE if an enemy was sighted
-
- When a player fires a missile, the point of impact becomes a fakeplayer
- so that monsters that see the impact will respond as if they had seen the
- player.
-
- To avoid spending too much time, only a single client (or fakeclient) is
- checked each frame. This means multi player games will have slightly
- slower noticing monsters.
-}
findTarget :: EdictReference -> Quake Bool
findTarget selfRef@(EdictReference selfIdx) = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy /= 0
      then
        -- we skip this chunk of code here cause we always returns False
        {-
          if (self.goalentity != null && self.goalentity.inuse
                && self.goalentity.classname != null) {
            if (self.goalentity.classname.equals("target_actor"))
                return false;
          }
        -}

        -- FIXME: look for monsters?
        return False
      else
        checkCombatPoint self

  where checkCombatPoint :: EdictT -> Quake Bool
        checkCombatPoint self = do
          -- if we're going to a combat point, just proceed
          if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiCombatPoint /= 0
            then return False
            else checkHearNoise self

        checkHearNoise :: EdictT -> Quake Bool
        checkHearNoise self = do
          -- if the first spawnflag bit is set, the monster will only wake up on
          -- really seeing the player, not another monster getting angry or
          -- hearing something
          -- revised behavior so they will wake up if they "see" a player make a
          -- noise but not weapon impact/explosion noises
          level <- use $ gameBaseGlobals.gbLevel

          if | (level^.llSightEntityFrameNum) >= ((level^.llFrameNum) - 1) && (self^.eSpawnFlags) .&. 1 == 0 -> do
                 let Just clientRef@(EdictReference clientIdx) = level^.llSightEntity
                 Just client <- preuse $ gameBaseGlobals.gbGEdicts.ix clientIdx
                 if (client^.eEdictOther.eoEnemy) == (self^.eEdictOther.eoEnemy)
                   then return False
                   else checkClientInUse clientRef False

             | (level^.llSoundEntityFrameNum) >= ((level^.llFrameNum) - 1) ->
                 checkClientInUse (fromJust $ level^.llSoundEntity) True

             | isJust (self^.eEdictOther.eoEnemy) && (level^.llSound2EntityFrameNum) >= ((level^.llFrameNum) - 1) && (self^.eSpawnFlags) .&. 1 /= 0 ->
                 checkClientInUse (fromJust $ level^.llSound2Entity) True

             | otherwise -> do
                 case level^.llSightClient of
                   Nothing -> return False -- no clients to get mad at
                   Just clientRef -> checkClientInUse clientRef False

        checkClientInUse :: EdictReference -> Bool -> Quake Bool
        checkClientInUse clientRef@(EdictReference clientIdx) heardIt = do
          -- if the entity went away, forget it
          Just client <- preuse $ gameBaseGlobals.gbGEdicts.ix clientIdx
          if not (client^.eInUse)
            then return False
            else checkClientFlags clientRef client heardIt

        checkClientFlags :: EdictReference -> EdictT -> Bool -> Quake Bool
        checkClientFlags clientRef client heardIt = do
          if | isJust (client^.eClient) ->
                 if (client^.eFlags) .&. Constants.flNoTarget /= 0
                   then return False
                   else actBasedOnHeardIt clientRef heardIt

             | (client^.eSvFlags) .&. Constants.svfMonster /= 0 ->
                 case client^.eEdictOther.eoEnemy of
                   Nothing -> return False
                   Just (EdictReference enemyIdx) -> do
                     Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx
                     if (enemy^.eFlags) .&. Constants.flNoTarget /= 0
                       then return False
                       else actBasedOnHeardIt clientRef heardIt

             | heardIt -> do
                 let Just (EdictReference ownerIdx) = client^.eOwner
                 Just owner <- preuse $ gameBaseGlobals.gbGEdicts.ix ownerIdx

                 if (owner^.eFlags) .&. Constants.flNoTarget /= 0
                   then return False
                   else actBasedOnHeardIt clientRef heardIt

             | otherwise -> return False

        actBasedOnHeardIt :: EdictReference -> Bool -> Quake Bool
        actBasedOnHeardIt clientRef@(EdictReference clientIdx) heardIt = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          Just client <- preuse $ gameBaseGlobals.gbGEdicts.ix clientIdx

          if not heardIt
            then do
              let r = range self client

              if | r == Constants.rangeFar ->
                     return False
                 | client^.eLightLevel <= 5 ->
                     return False

                 | otherwise -> do
                     vis <- visible selfRef clientRef
                     levelTime <- use $ gameBaseGlobals.gbLevel.llTime

                     if | not vis ->
                            return False

                        | r == Constants.rangeNear && fromIntegral (client^.eEdictStatus.eShowHostile) < levelTime && not (inFront self client) ->
                            return False

                        | r == Constants.rangeMid && not (inFront self client) ->
                            return False

                        | Just clientRef == (self^.eEdictOther.eoEnemy) ->
                            return True -- JDC false

                        | otherwise -> do
                            gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoEnemy .= Just clientRef

                            if (client^.eClassName) /= "player_noise"
                              then do
                                gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiSoundTarget))

                                case client^.eClient of
                                  Nothing -> do
                                    gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoEnemy .= client^.eEdictOther.eoEnemy
                                    let Just (EdictReference enemyIdx) = client^.eEdictOther.eoEnemy
                                    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

                                    case enemy^.eClient of
                                      Nothing -> do
                                        gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoEnemy .= Nothing
                                        return False

                                      _ -> finishFindTarget

                                  _ -> finishFindTarget
                              else
                                finishFindTarget
            else do
              -- heard it
              vis <- visible selfRef clientRef

              if (self^.eSpawnFlags) .&. 1 /= 0 && not vis
                then return False
                else do
                  inPHS <- use $ gameBaseGlobals.gbGameImport.giInPHS
                  v <- inPHS (self^.eEntityState.esOrigin) (client^.eEntityState.esOrigin)

                  if not v
                    then return False
                    else do
                      let temp = (client^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)

                      if norm temp > 1000 -- too far to hear
                        then return False
                        else do
                          -- check area portals - if they are different and
                          -- not connected then we can't hear it
                          done <- if (client^.eAreaNum) /= (self^.eAreaNum)
                                    then do
                                      areasConnected <- use $ gameBaseGlobals.gbGameImport.giAreasConnected
                                      connected <- areasConnected (self^.eAreaNum) (client^.eAreaNum)
                                      if not connected
                                        then return True
                                        else return False
                                    else
                                      return False

                          if done
                            then return False
                            else do
                              gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictPhysics.eIdealYaw .= Math3D.vectorYaw temp
                              M.changeYaw selfRef

                              -- hunt the sound for a bit; hopefully find
                              -- the real player
                              gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiSoundTarget)

                              if Just clientRef == (self^.eEdictOther.eoEnemy)
                                then return True
                                else do
                                  gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoEnemy .= Just clientRef
                                  finishFindTarget

        finishFindTarget :: Quake Bool
        finishFindTarget = do
          -- got one
          foundTarget selfRef

          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          when ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiSoundTarget == 0 && isJust (self^.eMonsterInfo.miSight)) $
            void $ entInteract (fromJust $ self^.eMonsterInfo.miSight) selfRef (fromJust $ self^.eEdictOther.eoEnemy) -- TODO: are we sure eoEnemy is Just ?

          return True

inFront :: EdictT -> EdictT -> Bool
inFront self other = undefined -- TODO GameUtil.inFront

foundTarget :: EdictReference -> Quake ()
foundTarget _ = do
    io (putStrLn "GameUtil.foundTarget") >> undefined -- TODO
