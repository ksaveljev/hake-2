{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameUtil ( spawn
                     , initEdict
                     , clearEdict
                     , freeEdict
                     , range
                     , useTargets
                     , freeEdictA
                     , monsterUse
                     , mCheckAttack
                     , killBox
                     , visible
                     , findTarget
                     , inFront
                     , foundTarget
                     , attackFinished
                     , onSameTeam
                     , megaHealthThink
                     , validateSelectedItem
                     ) where

import Control.Lens ((^.), use, (.=), (+=), zoom, (&), (.~), (%~), (+~), (-~), preuse, ix)
import Control.Monad (liftM, when, unless, void)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (norm, normalize, dot, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV

import Game.TraceT
import Game.EntityStateT
import Game.EdictT
import Game.GClientT
import Game.MonsterInfoT
import Game.ClientPersistantT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import {-# SOURCE #-} qualified Game.GameAI as GameAI
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import {-# SOURCE #-} qualified Game.GameCombat as GameCombat
import qualified Game.GameItems as GameItems
import qualified Game.Info as Info
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

{-
- Either finds a free edict, or allocates a new one. Try to avoid reusing
- an entity that was recently freed, because it can cause the client to
- think the entity morphed into something else instead of being removed and
- recreated, which can cause interpolated angles and bad trails.
-}
spawn :: Quake (Ref EdictT)
spawn = do
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
    numEdicts <- use $ gameBaseGlobals.gbNumEdicts
    edicts <- use $ gameBaseGlobals.gbGEdicts
    time <- use $ gameBaseGlobals.gbLevel.llTime

    -- search for a free edict starting from index (maxClientsValue+1) up
    -- to (numEdicts-1)
    foundIndex <- io $ findFreeEdict (MV.drop (maxClientsValue + 1) edicts) time maxClientsValue numEdicts

    case foundIndex of
      Just idx -> do
        let newRef = Ref idx
        writeRef newRef (newEdictT idx)
        initEdict newRef
        return newRef

      Nothing -> do
        maxEntities <- use $ gameBaseGlobals.gbGame.glMaxEntities

        when (numEdicts == maxEntities) $ do
          err <- use $ gameBaseGlobals.gbGameImport.giError
          err "ED_Alloc: no free edicts"

        let newRef = Ref numEdicts
        writeRef newRef (newEdictT numEdicts)
        gameBaseGlobals.gbNumEdicts += 1
        initEdict newRef
        return newRef

  where findFreeEdict :: MV.IOVector EdictT -> Float -> Int -> Int -> IO (Maybe Int)
        findFreeEdict edicts levelTime maxClientsValue numEdicts = do
          found <- findIndex edicts (\edict -> (not $ edict^.eInUse) && ((edict^.eFreeTime) < 2 || levelTime - (edict^.eFreeTime) > 0.5)) 0 (MV.length edicts)
          return $ case found of
                     Nothing -> Nothing
                     Just idx -> if idx + maxClientsValue + 1 >= numEdicts
                                   then Nothing
                                   else Just (idx + maxClientsValue + 1)

        findIndex :: MV.IOVector EdictT -> (EdictT -> Bool) -> Int -> Int -> IO (Maybe Int)
        findIndex edicts p idx maxIdx
          | idx >= maxIdx = return Nothing
          | otherwise = do
              edict <- MV.read edicts idx
              if p edict
                then return (Just idx)
                else findIndex edicts p (idx + 1) maxIdx

initEdict :: Ref EdictT -> Quake ()
initEdict edictRef = do
    edict <- readRef edictRef

    modifyRef edictRef (\v -> v & eInUse .~ True
                                   & eClassName .~ "noclass"
                                   & eGravity .~ 1.0
                                   & eEntityState .~ (newEntityStateT (Just edictRef)) { _esNumber = (edict^.eIndex) })

{-
- Call after linking a new trigger in during gameplay to force all entities
- it covers to immediately touch it.
-}
clearEdict :: Ref EdictT -> Quake ()
clearEdict edictRef = do
    edict <- readRef edictRef
    writeRef edictRef (newEdictT (edict^.eIndex))

-- Marks the edict as free
freeEdict :: Ref EdictT -> Quake ()
freeEdict edictRef = do
    edict <- readRef edictRef

    unlinkEntity <- use $ gameBaseGlobals.gbGameImport.giUnlinkEntity
    unlinkEntity edictRef

    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

    when ((edict^.eIndex) > maxClientsValue + Constants.bodyQueueSize) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      writeRef edictRef (newEdictT (edict^.eIndex)) { _eClassName = "freed", _eFreeTime = levelTime, _eInUse = False }

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
useTargets :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
useTargets edictRef activatorRef = do
    edict <- readRef edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex
        centerPrintf = gameImport^.giCenterPrintf

    -- check for a delay
    if (edict^.eDelay) /= 0
      then do
        -- create a temp object to fire at a later time
        tmpRef <- spawn
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        when (isNothing activatorRef) $ do
          dprintf "Think_Delay with no activator\n"

        modifyRef tmpRef (\v -> v & eClassName .~ "DelayedUse"
                                     & eNextThink .~ levelTime + (edict^.eDelay)
                                     & eThink .~ Just thinkDelay
                                     & eActivator .~ activatorRef
                                     & eMessage .~ (edict^.eMessage)
                                     & eTarget .~ (edict^.eTarget)
                                     & eKillTarget .~ (edict^.eKillTarget))

      else do
        let actRef = fromJust activatorRef
        activator <- readRef actRef

        -- print the message
        when (isJust (edict^.eMessage) && ((activator^.eSvFlags) .&. Constants.svfMonster) == 0) $ do
          centerPrintf actRef (fromJust (edict^.eMessage))

          if (edict^.eNoiseIndex) /= 0
            then
              sound (Just actRef) Constants.chanAuto (edict^.eNoiseIndex) 1 Constants.attnNorm 0

            else do
              talkIdx <- soundIndex (Just "misc/talk1.wav")
              sound (Just actRef) Constants.chanAuto talkIdx 1 Constants.attnNorm 0

        -- kill killtargets
        done <- if (isJust (edict^.eKillTarget))
                  then killKillTargets Nothing (fromJust $ edict^.eKillTarget)
                  else return False

        unless done $ do
          -- fire targets
          when (isJust $ edict^.eTarget) $ do
            fireTargets (BC.map toLower (edict^.eClassName)) Nothing GameBase.findByTarget (fromJust $ edict^.eTarget)

  where killKillTargets :: Maybe (Ref EdictT) -> B.ByteString -> Quake Bool
        killKillTargets entRef killTarget = do
          nextRef <- GameBase.gFind entRef GameBase.findByTarget killTarget

          case nextRef of
            Just newRef -> do
              freeEdict newRef
              edict <- readRef edictRef

              if (edict^.eInUse)
                then
                  killKillTargets nextRef killTarget

                else do
                  dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
                  dprintf "entity was removed while using killtargets\n"
                  return True

            Nothing ->
              return False

        fireTargets :: B.ByteString -> Maybe (Ref EdictT) -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake ()
        fireTargets edictClassName ref findBy targetName = do
          foundRef <- GameBase.gFind ref findBy targetName

          when (isJust foundRef) $ do
            let Just foundEdictRef = foundRef
            foundEdict <- readRef foundEdictRef

            -- doors fire area portals in a specific way
            let foundEdictClassName = BC.map toLower (foundEdict^.eClassName)

            if foundEdictClassName == "func_areaportal" && (any (== edictClassName) ["func_door", "func_door_rotating"])
              then
                fireTargets edictClassName foundRef findBy targetName

              else do
                dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf

                if foundEdictRef == edictRef
                  then
                    dprintf "WARNING: Entity used iteself.\n"

                  else
                    when (isJust $ foundEdict^.eUse) $
                      entUse (fromJust $ foundEdict^.eUse) foundEdictRef (Just edictRef) activatorRef

                edict <- readRef edictRef

                if not (edict^.eInUse)
                  then dprintf "entity was removed while using targets\n"
                  else fireTargets edictClassName foundRef findBy targetName

thinkDelay :: EntThink
thinkDelay =
  GenericEntThink "Think_Delay" $ \edictRef -> do
    edict <- readRef edictRef
    useTargets edictRef (edict^.eActivator)
    freeEdict edictRef
    return True

freeEdictA :: EntThink
freeEdictA =
  GenericEntThink "G_FreeEdictA" $ \edictRef -> do
    freeEdict edictRef
    return False

monsterUse :: EntUse
monsterUse =
  GenericEntUse "monster_use" $ \selfRef _ (Just activatorRef) -> do
    self <- readRef selfRef
    activator <- readRef activatorRef

    let done = isJust (self^.eEnemy) || (self^.eHealth) <= 0 || ((activator^.eFlags) .&. Constants.flNoTarget) /= 0 || (isNothing (activator^.eClient) && ((activator^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy) == 0)
    
    unless done $ do
      modifyRef selfRef (\v -> v & eEnemy .~ Just activatorRef)
      foundTarget selfRef

mCheckAttack :: EntThink
mCheckAttack =
  GenericEntThink "M_CheckAttack" $ \selfRef -> do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy

    checkEnemyHealth selfRef enemyRef

  where checkEnemyHealth :: Ref EdictT -> Ref EdictT -> Quake Bool
        checkEnemyHealth selfRef enemyRef = do
          self <- readRef selfRef
          enemy <- readRef enemyRef

          if (enemy^.eHealth) > 0
            then do
              let spot1 = (self^.eEntityState.esOrigin) & _z +~ fromIntegral (self^.eViewHeight)
                  spot2 = (enemy^.eEntityState.esOrigin) & _z +~ fromIntegral (enemy^.eViewHeight)

              trace <- use $ gameBaseGlobals.gbGameImport.giTrace
              traceT <- trace spot1 Nothing Nothing spot2 (Just selfRef) (Constants.contentsSolid .|. Constants.contentsMonster .|. Constants.contentsSlime .|. Constants.contentsLava .|. Constants.contentsWindow)

              -- do we have a clear shot?
              if (traceT^.tEnt) /= (self^.eEnemy)
                then return False
                else meleeAttack selfRef

            else
              meleeAttack selfRef

        meleeAttack :: Ref EdictT -> Quake Bool
        meleeAttack selfRef = do
          enemyRange <- use $ gameBaseGlobals.gbEnemyRange
          self <- readRef selfRef

          if enemyRange == Constants.rangeMelee
            then do
              -- don't always melee in easy mode
              skillValue <- liftM (^.cvValue) skillCVar
              r <- Lib.rand

              if skillValue == 0 && (r .&. 3) /= 0
                then
                  return False
                else do
                  let attackState = case self^.eMonsterInfo.miMelee of
                                      Just _ -> Constants.asMelee
                                      Nothing -> Constants.asMissile

                  modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ attackState)
                  return True

            else
              missileAttack selfRef

        missileAttack :: Ref EdictT -> Quake Bool
        missileAttack selfRef = do
          self <- readRef selfRef
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime
          enemyRange <- use $ gameBaseGlobals.gbEnemyRange

          if | isNothing (self^.eMonsterInfo.miAttack) -> return False
             | levelTime < (self^.eMonsterInfo.miAttackFinished) -> return False
             | enemyRange == Constants.rangeFar -> return False
             | otherwise -> do
                 let maybeChance = if | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0 -> Just 0.4
                                      | enemyRange == Constants.rangeMelee -> Just 0.2
                                      | enemyRange == Constants.rangeNear -> Just 0.1
                                      | enemyRange == Constants.rangeMid -> Just 0.02
                                      | otherwise -> Nothing

                 case maybeChance of
                   Nothing ->
                     return False

                   Just chance -> do
                     skillValue <- liftM (^.cvValue) skillCVar
                     let chance' = if | skillValue == 0 -> chance * 0.5
                                      | skillValue >= 2 -> chance * 2
                                      | otherwise -> chance

                     r <- Lib.randomF

                     if r < chance'
                       then do
                         r' <- Lib.randomF

                         modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ Constants.asMissile
                                                       & eMonsterInfo.miAttackFinished .~ levelTime + 2 * r')

                         return True

                       else do
                         when ((self^.eFlags) .&. Constants.flFly /= 0) $ do
                           r' <- Lib.randomF

                           let attackState = if r' < 0.3
                                               then Constants.asSliding
                                               else Constants.asStraight

                           modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ attackState)

                         return False

{-
- Kills all entities that would touch the proposed new positioning of ent.
- Ent should be unlinked before calling this!
-}
killBox :: Ref EdictT -> Quake Bool
killBox edictRef = do
    edict <- readRef edictRef

    trace <- use $ gameBaseGlobals.gbGameImport.giTrace
    traceT <- trace (edict^.eEntityState.esOrigin)
                    (Just $ edict^.eMins)
                    (Just $ edict^.eMaxs)
                    (edict^.eEntityState.esOrigin)
                    Nothing
                    Constants.maskPlayerSolid

    if isNothing (traceT^.tEnt) || (traceT^.tEnt) == Just worldRef
      then
        return True

      else do
        -- nail it
        v3o <- use $ globals.gVec3Origin
        let Just traceEntRef = traceT^.tEnt

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
        traceEnt <- readRef traceEntRef

        if (traceEnt^.eSolid) /= 0
          then return False
          else killBox edictRef

-- Returns 1 if the entity is visible to self, even if not infront()
visible :: Ref EdictT -> Ref EdictT -> Quake Bool
visible selfRef otherRef = do
    self <- readRef selfRef
    other <- readRef otherRef

    let spot1 = (self^.eEntityState.esOrigin) & _z +~ fromIntegral (self^.eViewHeight)
        spot2 = (other^.eEntityState.esOrigin) & _z +~ fromIntegral (other^.eViewHeight)

    v3o <- use $ globals.gVec3Origin
    trace <- use $ gameBaseGlobals.gbGameImport.giTrace
    traceT <- trace spot1 (Just v3o) (Just v3o) spot2 (Just selfRef) Constants.maskOpaque

    return $ (traceT^.tFraction) == 1

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
findTarget :: Ref EdictT -> Quake Bool
findTarget selfRef = do
    self <- readRef selfRef

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
                 let Just clientRef = level^.llSightEntity
                 client <- readRef clientRef
                 if (client^.eEnemy) == (self^.eEnemy)
                   then return False
                   else checkClientInUse clientRef False

             | (level^.llSoundEntityFrameNum) >= ((level^.llFrameNum) - 1) ->
                 checkClientInUse (fromJust $ level^.llSoundEntity) True

             | isJust (self^.eEnemy) && (level^.llSound2EntityFrameNum) >= ((level^.llFrameNum) - 1) && (self^.eSpawnFlags) .&. 1 /= 0 ->
                 checkClientInUse (fromJust $ level^.llSound2Entity) True

             | otherwise -> do
                 case level^.llSightClient of
                   Nothing -> return False -- no clients to get mad at
                   Just clientRef -> checkClientInUse clientRef False

        checkClientInUse :: Ref EdictT -> Bool -> Quake Bool
        checkClientInUse clientRef heardIt = do
          -- if the entity went away, forget it
          client <- readRef clientRef
          if not (client^.eInUse)
            then return False
            else checkClientFlags clientRef client heardIt

        checkClientFlags :: Ref EdictT -> EdictT -> Bool -> Quake Bool
        checkClientFlags clientRef client heardIt = do
          if | isJust (client^.eClient) ->
                 if (client^.eFlags) .&. Constants.flNoTarget /= 0
                   then return False
                   else actBasedOnHeardIt clientRef heardIt

             | (client^.eSvFlags) .&. Constants.svfMonster /= 0 ->
                 case client^.eEnemy of
                   Nothing -> return False
                   Just enemyRef -> do
                     enemy <- readRef enemyRef
                     if (enemy^.eFlags) .&. Constants.flNoTarget /= 0
                       then return False
                       else actBasedOnHeardIt clientRef heardIt

             | heardIt -> do
                 let Just ownerRef = client^.eOwner
                 owner <- readRef ownerRef

                 if (owner^.eFlags) .&. Constants.flNoTarget /= 0
                   then return False
                   else actBasedOnHeardIt clientRef heardIt

             | otherwise -> return False

        actBasedOnHeardIt :: Ref EdictT -> Bool -> Quake Bool
        actBasedOnHeardIt clientRef heardIt = do
          self <- readRef selfRef
          client <- readRef clientRef

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

                        | r == Constants.rangeNear && fromIntegral (client^.eShowHostile) < levelTime && not (inFront self client) ->
                            return False

                        | r == Constants.rangeMid && not (inFront self client) ->
                            return False

                        | Just clientRef == (self^.eEnemy) ->
                            return True -- JDC false

                        | otherwise -> do
                            modifyRef selfRef (\v -> v & eEnemy .~ Just clientRef)

                            if (client^.eClassName) /= "player_noise"
                              then do
                                modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiSoundTarget)))

                                case client^.eClient of
                                  Nothing -> do
                                    modifyRef selfRef (\v -> v & eEnemy .~ (client^.eEnemy))

                                    let Just enemyRef = client^.eEnemy
                                    enemy <- readRef enemyRef

                                    case enemy^.eClient of
                                      Nothing -> do
                                        modifyRef selfRef (\v -> v & eEnemy .~ Nothing)
                                        return False

                                      _ -> finishFindTarget

                                  _ -> finishFindTarget
                              else
                                finishFindTarget
            else do
              -- heard it
              vis <- visible selfRef clientRef

              if (self^.eSpawnFlags) .&. 1 /= 0 && not vis
                then
                  return False

                else do
                  inPHS <- use $ gameBaseGlobals.gbGameImport.giInPHS
                  v <- inPHS (self^.eEntityState.esOrigin) (client^.eEntityState.esOrigin)

                  if not v
                    then
                      return False

                    else do
                      let temp = (client^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)

                      if norm temp > 1000 -- too far to hear
                        then
                          return False

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
                            then
                              return False

                            else do
                              modifyRef selfRef (\v' -> v' & eIdealYaw .~ Math3D.vectorYaw temp)
                              M.changeYaw selfRef

                              -- hunt the sound for a bit; hopefully find
                              -- the real player
                              modifyRef selfRef (\v' -> v' & eMonsterInfo.miAIFlags %~ (.|. Constants.aiSoundTarget))

                              if Just clientRef == (self^.eEnemy)
                                then
                                  return True

                                else do
                                  modifyRef selfRef (\v' -> v' & eEnemy .~ Just clientRef)
                                  finishFindTarget

        finishFindTarget :: Quake Bool
        finishFindTarget = do
          -- got one
          foundTarget selfRef

          self <- readRef selfRef

          when ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiSoundTarget == 0 && isJust (self^.eMonsterInfo.miSight)) $
            void $ entInteract (fromJust $ self^.eMonsterInfo.miSight) selfRef (fromJust $ self^.eEnemy) -- RESEARCH: are we sure eEnemy is Just ?

          return True

-- Returns true if the entity is in front (in sight) of self
inFront :: EdictT -> EdictT -> Bool
inFront self other =
    let (Just forward, _, _) = Math3D.angleVectors (self^.eEntityState.esAngles) True False False
        vec = normalize ((other^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin))
        dot' = vec `dot` forward
    in dot' > 0.3

foundTarget :: Ref EdictT -> Quake ()
foundTarget selfRef = do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef

    -- let other monsters see this monster for a while
    when (isJust (enemy^.eClient)) $ do
      frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

      zoom (gameBaseGlobals.gbLevel) $ do
        llSightEntity .= Just selfRef
        llSightEntityFrameNum .= frameNum

      modifyRef selfRef (\v -> v & eLightLevel .~ 128)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime


    modifyRef selfRef (\v -> v & eShowHostile .~ truncate levelTime + 1 -- wake up other monsters
                                  & eMonsterInfo.miLastSighting .~ (enemy^.eEntityState.esOrigin)
                                  & eMonsterInfo.miTrailTime .~ levelTime)

    case self^.eCombatTarget of
      Nothing ->
        GameAI.huntTarget selfRef

      Just combatTarget -> do
        target <- GameBase.pickTarget (self^.eCombatTarget)

        case target of
          Nothing -> do
            modifyRef selfRef (\v -> v & eGoalEntity .~ (self^.eEnemy)
                                          & eMoveTarget .~ (self^.eEnemy))

            GameAI.huntTarget selfRef

            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf ((self^.eClassName) `B.append`
                     " at " `B.append`
                     Lib.vtos (self^.eEntityState.esOrigin) `B.append`
                     ", combattarget " `B.append`
                     combatTarget `B.append`
                     " not found\n")

          Just _ -> do
            modifyRef selfRef (\v -> v & eGoalEntity .~ target
                                          & eMoveTarget .~ target
                                          -- clear out our combattarget, these are a one shot deal
                                          & eCombatTarget .~ Nothing
                                          & eMonsterInfo.miAIFlags %~ (.|. Constants.aiCombatPoint)
                                          & eMonsterInfo.miPauseTime .~ 0)

            -- clear the targetname, that point is ours!
            let Just moveTargetRef = target
            modifyRef moveTargetRef (\v -> v & eTargetName .~ Nothing)

            -- run for it
            void $ think (fromJust $ self^.eMonsterInfo.miRun) selfRef

attackFinished :: Ref EdictT -> Float -> Quake ()
attackFinished selfRef time = do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyRef selfRef (\v -> v & eMonsterInfo.miAttackFinished .~ levelTime + time)

-- Returns true, if two edicts are on the same team.
onSameTeam :: Ref EdictT -> Ref EdictT -> Quake Bool
onSameTeam edictRef otherRef = do
    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar
    
    if dmFlagsValue .&. (Constants.dfModelTeams .|. Constants.dfSkinTeams) == 0
      then return False
      else do
        edictTeam <- clientTeam edictRef
        otherTeam <- clientTeam otherRef
        
        return (edictTeam == otherTeam)

-- Returns the team string of an entity 
-- with respect to rteam_by_model and team_by_skin. 
clientTeam :: Ref EdictT -> Quake B.ByteString
clientTeam edictRef = do
    edict <- readRef edictRef
    
    case edict^.eClient of
      Nothing ->
        return ""
      
      Just (Ref gClientIdx) -> do
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
        value <- Info.valueForKey (gClient^.gcPers.cpUserInfo) "skin"
        
        case '/' `BC.elemIndex` value of
          Nothing ->
            return value
          
          Just idx -> do
            dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar
            
            return $ if dmFlagsValue .&. Constants.dfModelTeams /= 0
                       then B.take idx value
                       else B.drop (idx + 1) value

megaHealthThink :: EntThink
megaHealthThink =
  GenericEntThink "MegaHealth_think" $ \selfRef -> do
    self <- readRef selfRef
    let Just ownerRef = self^.eOwner
    owner <- readRef ownerRef
    
    if (owner^.eHealth) > (owner^.eMaxHealth)
      then do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        modifyRef selfRef (\v -> v & eNextThink .~ levelTime + 1)
        modifyRef ownerRef (\v -> v & eHealth -~ 1)
        return False
        
      else do
        deathmatchValue <- liftM (^.cvValue) deathmatchCVar
        
        if not ((self^.eSpawnFlags) .&. Constants.droppedItem /= 0) && deathmatchValue /= 0
          then
            GameItems.setRespawn selfRef 20
          else
            freeEdict selfRef
        
        return False

validateSelectedItem :: Ref EdictT -> Quake ()
validateSelectedItem edictRef = do
    edict <- readRef edictRef
    
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    
    if (gClient^.gcPers.cpInventory) UV.! (gClient^.gcPers.cpSelectedItem) /= 0
      then return () -- valid
      else GameItems.selectNextItem edictRef (-1)
