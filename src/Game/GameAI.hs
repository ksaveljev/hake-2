{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameAI ( aiSetSightClient
                   , huntTarget
                   , aiStand
                   , aiWalk
                   , aiRun
                   , aiCharge
                   , aiMove
                   , aiTurn
                   , walkMonsterStart
                   , swimMonsterStart
                   , flyMonsterStart
                   ) where

import Control.Lens (use, (^.), ix, preuse, (.=), (%=), zoom, (&), (.~), (%~))
import Control.Monad (void, when, unless, liftM)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), norm, _y)
import qualified Data.ByteString as B

import Game.GClientT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import qualified Game.GameUtil as GameUtil
import qualified Game.Monster as Monster
import qualified Game.PlayerTrail as PlayerTrail
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

{-
- Don't move, but turn towards ideal_yaw Distance is for slight position
- adjustments needed by the animations.
-}
aiTurn :: AI
aiTurn =
  GenericAI "ai_turn" $ \selfRef dist -> do
    when (dist /= 0) $ do
      self <- readRef selfRef
      void $ M.walkMove selfRef (self^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) dist

    v <- GameUtil.findTarget selfRef

    unless v $
      M.changeYaw selfRef

{-
- Used for standing around and looking for players Distance is for slight
- position adjustments needed by the animations. 
-}
aiStand :: AI
aiStand =
  GenericAI "ai_stand" $ \selfRef dist -> do
    when (dist /= 0) $ do
      self <- readRef selfRef
      void $ M.walkMove selfRef (self^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) dist

    checkAIStandGround selfRef
      >>= checkFindTarget selfRef
      >>= checkPauseTime selfRef
      >>= tryToIdle selfRef

  where checkAIStandGround :: Ref EdictT -> Quake Bool
        checkAIStandGround selfRef = do
          self <- readRef selfRef

          if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
            then do
              case self^.eEnemy of
                Just enemyRef -> do
                  enemy <- readRef enemyRef
                  let v = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
                      idealYaw = Math3D.vectorYaw v

                  modifyRef selfRef (\v -> v & eIdealYaw .~ idealYaw)

                  when ((self^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) /= idealYaw && (self^.eMonsterInfo.miAIFlags) .&. Constants.aiTempStandGround /= 0) $ do
                    modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement (Constants.aiStandGround .|. Constants.aiTempStandGround))))
                    void $ think (fromJust $ self^.eMonsterInfo.miRun) selfRef

                  M.changeYaw selfRef
                  void $ aiCheckAttack selfRef 0

                Nothing ->
                  void $ GameUtil.findTarget selfRef
              
              return True
            else
              return False

        checkFindTarget :: Ref EdictT -> Bool -> Quake Bool
        checkFindTarget _ True = return True
        checkFindTarget selfRef _ = GameUtil.findTarget selfRef

        checkPauseTime :: Ref EdictT -> Bool -> Quake Bool
        checkPauseTime _ True = return True
        checkPauseTime selfRef _ = do
          self <- readRef selfRef
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          if levelTime > (self^.eMonsterInfo.miPauseTime)
            then do
              void $ think (fromJust $ self^.eMonsterInfo.miWalk) selfRef
              return True
            else
              return False

        tryToIdle :: Ref EdictT -> Bool -> Quake ()
        tryToIdle _ True = return ()
        tryToIdle selfRef _ = do
          self <- readRef selfRef
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          when ((self^.eSpawnFlags) .&. 1 == 0 && isJust (self^.eMonsterInfo.miIdle) && levelTime > (self^.eMonsterInfo.miIdleTime)) $ do
            rf <- Lib.randomF

            if (self^.eMonsterInfo.miIdleTime) /= 0
              then do
                void $ think (fromJust $ self^.eMonsterInfo.miIdle) selfRef
                modifyRef selfRef (\v -> v & eMonsterInfo.miIdleTime .~ levelTime + 15 + rf * 15)
              else do
                modifyRef selfRef (\v -> v & eMonsterInfo.miIdleTime .~ levelTime + rf * 15)

-- Turns towards target and advances
-- Use this call with a distance of 0 to replace ai_face
aiCharge :: AI
aiCharge =
  GenericAI "ai_charge" $ \selfRef dist -> do
    self <- readRef selfRef
    let Just enemyRef = self^.eEnemy
    enemy <- readRef enemyRef

    let a = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)

    modifyRef selfRef (\v -> v & eIdealYaw .~ Math3D.vectorYaw a)
    M.changeYaw selfRef

    when (dist /= 0) $ do
      self' <- readRef selfRef
      void $ M.walkMove selfRef (self'^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) dist

-- Move the specified distance at current facing. This replaces the QC
-- functions: ai_forward, ai_back, ai_pain, and ai_painforward
aiMove :: AI
aiMove =
  GenericAI "ai_move" $ \selfRef dist -> do
    self <- readRef selfRef
    void $ M.walkMove selfRef (self^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) dist

-- The monster is walking it's beat.
aiWalk :: AI
aiWalk =
  GenericAI "ai_walk" $ \selfRef dist -> do
    -- io (print "GameAI.aiWalk!")
    -- io (print $ "dist = " ++ show dist)
    M.moveToGoal selfRef dist

    -- check for noticing a player
    found <- GameUtil.findTarget selfRef

    unless found $ do
      self <- readRef selfRef
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime

      when (isJust (self^.eMonsterInfo.miSearch) && levelTime > (self^.eMonsterInfo.miIdleTime)) $ do
        r <- Lib.randomF

        if (self^.eMonsterInfo.miIdleTime) /= 0
          then do
            void $ think (fromJust $ self^.eMonsterInfo.miSearch) selfRef
            modifyRef selfRef (\v -> v & eMonsterInfo.miIdleTime .~ levelTime + 15 + r * 15)
          else
            modifyRef selfRef (\v -> v & eMonsterInfo.miIdleTime .~ levelTime + r * 15)

-- The monster has an enemy it is trying to kill.
aiRun :: AI
aiRun =
  GenericAI "ai_run" $ \selfRef dist -> do
    self <- readRef selfRef

    -- if we're going to a combat point, just proceed
    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiCombatPoint /= 0
      then
        M.moveToGoal selfRef dist
      else do
        if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiSoundTarget /= 0
          then do
            let Just enemyRef = self^.eEnemy
            enemy <- readRef enemyRef
            let v = (self^.eEntityState.esOrigin) - (enemy^.eEntityState.esOrigin)
            -- ...and reached it
            if norm v < 64
              then do
                -- don't move, just stand and listen
                void $ think (fromJust $ self^.eMonsterInfo.miStand) selfRef
                -- since now it is aware and does not need to be triggered again.
                modifyRef selfRef (\v -> v & eSpawnFlags %~ (.&. (complement 1))
                                              & eEnemy .~ Nothing)
              else
                M.moveToGoal selfRef dist

            -- look for new targets
            ok <- GameUtil.findTarget selfRef
            when ok $
              checkAttack selfRef dist
          else
            checkAttack selfRef dist

  where checkAttack :: Ref EdictT -> Float -> Quake ()
        checkAttack selfRef dist = do
          attack <- aiCheckAttack selfRef dist
          unless attack $
            checkSliding selfRef dist

        checkSliding :: Ref EdictT -> Float -> Quake ()
        checkSliding selfRef dist = do
          self <- readRef selfRef

          if (self^.eMonsterInfo.miAttackState) == Constants.asSliding
            then aiRunSlide selfRef dist
            else checkEnemyVis selfRef dist

        checkEnemyVis :: Ref EdictT -> Float -> Quake ()
        checkEnemyVis selfRef dist = do
          enemyVis <- use $ gameBaseGlobals.gbEnemyVis

          if enemyVis
            then do
              M.moveToGoal selfRef dist

              self <- readRef selfRef
              let Just enemyRef = self^.eEnemy
              enemy <- readRef enemyRef
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime

              modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiLostSight))
                                            & eMonsterInfo.miLastSighting .~ (enemy^.eEntityState.esOrigin)
                                            & eMonsterInfo.miTrailTime .~ levelTime)

            else
              checkCoopEnemy selfRef dist

        checkCoopEnemy :: Ref EdictT -> Float -> Quake ()
        checkCoopEnemy selfRef dist = do
          coopValue <- liftM (^.cvValue) coopCVar

          -- coop will change to another enemy if visible
          if coopValue /= 0
            then do
              -- FIXME: insane guys get mad with this, which causes crashes!
              ok <- GameUtil.findTarget selfRef
              unless ok $
                checkSearchTime selfRef dist
            else
              checkSearchTime selfRef dist

        checkSearchTime :: Ref EdictT -> Float -> Quake ()
        checkSearchTime selfRef dist = do
          self <- readRef selfRef
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          if (self^.eMonsterInfo.miSearchTime) /= 0 && levelTime > (self^.eMonsterInfo.miSearchTime + 20)
            then do
              M.moveToGoal selfRef dist
              modifyRef selfRef (\v -> v & eMonsterInfo.miSearchTime .~ 0)
            else
              proceedRun selfRef dist

        proceedRun :: Ref EdictT -> Float -> Quake ()
        proceedRun selfRef dist = do
          self <- readRef selfRef

          let save = self^.eGoalEntity
          tempGoal <- GameUtil.spawn

          modifyRef selfRef (\v -> v & eGoalEntity .~ Just tempGoal)

          new1 <- calcNew1 selfRef

          pursueNext selfRef dist new1

          M.moveToGoal selfRef dist

          GameUtil.freeEdict tempGoal
          modifyRef selfRef (\v -> v & eGoalEntity .~ save) -- TODO: jake2 checks for self != null here, do we need it?

        calcNew1 :: Ref EdictT -> Quake Bool
        calcNew1 selfRef = do
          self <- readRef selfRef

          new1 <- if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiLostSight == 0
                    then do
                      -- just lost sight of the player, decide where to go first
                      modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (\v -> (v .|. (Constants.aiLostSight .|. Constants.aiPursuitLastSeen)) .&. complement (Constants.aiPursueNext .|. Constants.aiPursueTemp)))
                      return True
                    else
                      return False

          self' <- readRef selfRef

          if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiPursueNext /= 0
            then do
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime

              modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiPursueNext))
                                              -- give ourself more time since we got this far
                                            & eMonsterInfo.miSearchTime .~ levelTime + 5)

              (maybeMarker, new1') <- if | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiPursueTemp /= 0 -> do
                                             modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiPursueTemp))
                                                                           & eMonsterInfo.miLastSighting .~ (self^.eMonsterInfo.miSavedGoal))

                                             return (Nothing, True)

                                         | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiPursuitLastSeen /= 0 -> do
                                             modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiPursuitLastSeen)))
                                             marker <- PlayerTrail.pickFirst selfRef
                                             return (marker, new1)

                                         | otherwise -> do
                                             marker <- PlayerTrail.pickNext selfRef
                                             return (marker, new1)

              case maybeMarker of
                Nothing ->
                  return new1'

                Just markerRef -> do
                  marker <- readRef markerRef

                  modifyRef selfRef (\v -> v & eMonsterInfo.miTrailTime .~ (marker^.eTimeStamp)
                                                & eEntityState.esAngles._y .~ (marker^.eEntityState.esAngles._y) -- IMPROVE: use Constants.yaw instead _y directly
                                                & eIdealYaw .~ (marker^.eEntityState.esAngles._y)) -- IMPROVE: use Constants.yaw instead _y directly

                  return True
            else
              return new1

        pursueNext :: Ref EdictT -> Float -> Bool -> Quake ()
        pursueNext selfRef dist new1 = do
          self <- readRef selfRef
          let v = (self^.eEntityState.esOrigin) - (self^.eMonsterInfo.miLastSighting)
              d1 = norm v
          dist' <- if d1 <= dist
                     then do
                       modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.|. Constants.aiPursueNext))
                       return d1
                     else
                       return dist

          let Just goalEntityRef = self^.eGoalEntity
          modifyRef goalEntityRef (\v -> v & eEntityState.esOrigin .~ (self^.eMonsterInfo.miLastSighting))

          correctCourse selfRef dist' new1

        correctCourse :: Ref EdictT -> Float -> Bool -> Quake ()
        correctCourse selfRef dist new1 = do
          when new1 $ do
            self <- readRef selfRef
            trace <- use $ gameBaseGlobals.gbGameImport.giTrace
            traceT <- trace (self^.eEntityState.esOrigin) (Just $ self^.eMins) (Just $ self^.eMaxs) (self^.eMonsterInfo.miLastSighting) (Just selfRef) Constants.maskPlayerSolid

            when ((traceT^.tFraction) < 1) $ do
              let Just goalEntityRef = self^.eGoalEntity
              goalEntity <- readRef goalEntityRef

              let v = (goalEntity^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
                  d1 = norm v
                  center = traceT^.tFraction
                  d2 = d1 * ((center + 1) / 2)
                  yaw = Math3D.vectorYaw v

              modifyRef selfRef (\v -> v & eEntityState.esAngles._y .~ yaw -- IMPROVE: use Constants.yaw instead of directly using _y
                                            & eIdealYaw .~ yaw)

              self' <- readRef selfRef

              let (Just vForward, Just vRight, _) = Math3D.angleVectors (self'^.eEntityState.esAngles) True True False
                  v' = V3 d2 (-16) 0
                  leftTarget = Math3D.projectSource (self'^.eEntityState.esOrigin) v' vForward vRight

              traceT' <- trace (self'^.eEntityState.esOrigin) (Just $ self'^.eMins) (Just $ self'^.eMaxs) leftTarget (Just selfRef) Constants.maskPlayerSolid
              let left = traceT'^.tFraction

              let v'' = V3 d2 16 0
                  rightTarget = Math3D.projectSource (self'^.eEntityState.esOrigin) v'' vForward vRight

              traceT'' <- trace (self'^.eEntityState.esOrigin) (Just $ self'^.eMins) (Just $ self'^.eMaxs) rightTarget (Just selfRef) Constants.maskPlayerSolid
              let right = traceT''^.tFraction
                  center' = d1 * center / d2

              if | left >= center' && left > right -> do
                     let leftTarget' = if left < 1
                                         then Math3D.projectSource (self'^.eEntityState.esOrigin) (V3 (d2 * left * 0.5) (-16) 0) vForward vRight
                                         else leftTarget

                     modifyRef selfRef (\v -> v & eMonsterInfo.miSavedGoal .~ (self'^.eMonsterInfo.miLastSighting)
                                                   & eMonsterInfo.miLastSighting .~ leftTarget'
                                                   & eMonsterInfo.miAIFlags %~ (.|. Constants.aiPursueTemp)
                                                   & eEntityState.esAngles._y .~ Math3D.vectorYaw (leftTarget' - (self'^.eEntityState.esOrigin)) -- IMPROVE: use Constants.yaw instead of using _y directly
                                                   & eIdealYaw .~ Math3D.vectorYaw (leftTarget' - (self'^.eEntityState.esOrigin)))

                     modifyRef goalEntityRef (\v -> v & eEntityState.esOrigin .~ leftTarget')

                 | right >= center' && right > left -> do
                     let rightTarget' = if right < 1
                                          then Math3D.projectSource (self'^.eEntityState.esOrigin) (V3 (d2 * right * 0.5) 16 0) vForward vRight
                                          else rightTarget

                     modifyRef selfRef (\v -> v & eMonsterInfo.miSavedGoal .~ (self'^.eMonsterInfo.miLastSighting)
                                                   & eMonsterInfo.miLastSighting .~ rightTarget'
                                                   & eMonsterInfo.miAIFlags %~ (.|. Constants.aiPursueTemp)
                                                   & eEntityState.esAngles._y .~ Math3D.vectorYaw (rightTarget' - (self'^.eEntityState.esOrigin))
                                                   & eIdealYaw .~ Math3D.vectorYaw (rightTarget' - (self'^.eEntityState.esOrigin)))

                     modifyRef goalEntityRef (\v -> v & eEntityState.esOrigin .~ rightTarget')

                 | otherwise ->
                     return ()

walkMonsterStart :: EntThink
walkMonsterStart =
  GenericEntThink "walkmonster_start" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eThink .~ Just walkMonsterStartGo)
    void $ Monster.monsterStart edictRef
    return True

{-
- Called once each frame to set level.sight_client to the player to be
- checked for in findtarget.
- 
- If all clients are either dead or in notarget, sight_client will be null.
- 
- In coop games, sight_client will cycle between the clients.
-}
aiSetSightClient :: Quake ()
aiSetSightClient = do
    sightClient <- use $ gameBaseGlobals.gbLevel.llSightClient

    start <- case sightClient of
               Nothing -> return 1
               Just edictRef -> do
                 edict <- readRef edictRef
                 return (edict^.eIndex)

    let check = start

    maxClientsValue <- use $ gameBaseGlobals.gbGame.glMaxClients

    lookThroughClients maxClientsValue start check

  where lookThroughClients :: Int -> Int -> Int -> Quake ()
        lookThroughClients maxClients start check = do
          let check' = if check + 1 > maxClients
                         then 1
                         else check + 1

          edict <- readRef (Ref check')

          if | (edict^.eInUse) && (edict^.eHealth) > 0 && (edict^.eFlags) .&. Constants.flNoTarget == 0 ->
                 gameBaseGlobals.gbLevel.llSightClient .= Just (Ref check') -- got one
             | check' == start ->
                 gameBaseGlobals.gbLevel.llSightClient .= Nothing
             | otherwise -> lookThroughClients maxClients start check'

walkMonsterStartGo :: EntThink
walkMonsterStartGo =
  GenericEntThink "walkmonster_start_go" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    self <- readRef selfRef

    let spawnFlags = self^.eSpawnFlags

    -- preuse (gameBaseGlobals.gbGEdicts.ix selfIdx) >>= \(Just blah) -> do
      -- io (print "GameAI: BEFORE")
      -- io (print $ "self.frame = " ++ show (blah^.eEntityState.esFrame))
      -- io (print $ "move.firstframe = " ++ show ((fromJust $ blah^.eMonsterInfo.miCurrentMove)^.mmFirstFrame))

    when (spawnFlags .&. 2 == 0 && levelTime < 1) $ do
      void $ think M.dropToFloor selfRef

      self' <- readRef selfRef
      let groundEntity = self'^.eGroundEntity

      when (isJust groundEntity) $ do
        ok <- M.walkMove selfRef 0 0

        when (not ok) $ do
          self'' <- readRef selfRef
          dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
          dprintf ((self''^.eClassName) `B.append` " in solid at " `B.append` (Lib.vtos (self''^.eEntityState.esOrigin)) `B.append` "\n")
          
    self' <- readRef selfRef
    let yawSpeed = self'^.eYawSpeed

    when (yawSpeed == 0) $
      modifyRef selfRef (\v -> v & eYawSpeed .~ 40)

    modifyRef selfRef (\v -> v & eViewHeight .~ 25)

    Monster.monsterStartGo selfRef

    self'' <- readRef selfRef
    let spawnFlags' = self''^.eSpawnFlags

    when (spawnFlags' .&. 2 /= 0) $
      void $ think Monster.monsterTriggeredStart selfRef

    -- preuse (gameBaseGlobals.gbGEdicts.ix selfIdx) >>= \(Just blah) -> do
      -- io (print "GameAI: AFTER")
      -- io (print $ "self.frame = " ++ show (blah^.eEntityState.esFrame))
      -- io (print $ "move.firstframe = " ++ show ((fromJust $ blah^.eMonsterInfo.miCurrentMove)^.mmFirstFrame))

    return True

{-
- Decides if we're going to attack or do something else used by ai_run and
- ai_stand.
- 
- .enemy Will be world if not currently angry at anyone.
- 
- .movetarget The next path spot to walk toward. If .enemy, ignore
- .movetarget. When an enemy is killed, the monster will try to return to
- it's path.
- 
- .hunt_time Set to time + something when the player is in sight, but
- movement straight for him is blocked. This causes the monster to use wall
- following code for movement direction instead of sighting on the player.
- 
- .ideal_yaw A yaw angle of the intended direction, which will be turned
- towards at up to 45 deg / state. If the enemy is in view and hunt_time is
- not active, this will be the exact line towards the enemy.
- 
- .pausetime A monster will leave it's stand state and head towards it's
- .movetarget when time > .pausetime.
- 
- walkmove(angle, speed) primitive is all or nothing
-}
aiCheckAttack :: Ref EdictT -> Float -> Quake Bool
aiCheckAttack selfRef dist = do
    -- this causes monsters to run blindly to the combat point w/o firing
    result <- checkBlindRun

    case result of
      Just b ->
        return b

      Nothing -> do
        gameBaseGlobals.gbEnemyVis .= False

        -- see if the enemy is dead
        hesDeadJim <- checkIfEnemyIsDead

        done <- if hesDeadJim
                  then lookForOtherTarget
                  else return False

        if done
          then
            return True

          else do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime

            modifyRef selfRef (\v -> v & eShowHostile .~ truncate levelTime + 1) -- wake up other

            -- monsters check knowledge of enemy
            self <- readRef selfRef
            enemyVis <- GameUtil.visible selfRef (fromJust $ self^.eEnemy)
            gameBaseGlobals.gbEnemyVis .= enemyVis

            let Just enemyRef = self^.eEnemy
            enemy <- readRef enemyRef

            when enemyVis $
              modifyRef selfRef (\v -> v & eMonsterInfo.miSearchTime .~ levelTime + 5
                                            & eMonsterInfo.miLastSighting .~ (enemy^.eEntityState.esOrigin))

            let enemyInFront = GameUtil.inFront self enemy
                enemyRange = GameUtil.range self enemy
                temp = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
                enemyYaw = Math3D.vectorYaw temp

            zoom gameBaseGlobals $ do
              gbEnemyInFront .= enemyInFront
              gbEnemyRange .= enemyRange
              gbEnemyYaw .= enemyYaw

            if | (self^.eMonsterInfo.miAttackState) == Constants.asMissile -> do
                   aiRunMissile selfRef
                   return True
               | (self^.eMonsterInfo.miAttackState) == Constants.asMelee -> do
                   aiRunMelee selfRef
                   return True
               | otherwise -> do
                   enemyVis' <- use $ gameBaseGlobals.gbEnemyVis

                   if not enemyVis'
                     then return False
                     else think (fromJust $ self^.eMonsterInfo.miCheckAttack) selfRef

  where checkBlindRun :: Quake (Maybe Bool)
        checkBlindRun = do
          self <- readRef selfRef

          case self^.eGoalEntity of
            Nothing ->
              return Nothing

            Just goalEntityRef -> do
              if | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiCombatPoint /= 0 ->
                     return (Just False)

                 | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiSoundTarget /= 0 -> do
                     levelTime <- use $ gameBaseGlobals.gbLevel.llTime
                     let Just enemyRef = self^.eEnemy
                     enemy <- readRef enemyRef

                     if levelTime - (enemy^.eTeleportTime) > 5.0
                       then do
                         when ((self^.eGoalEntity) == (self^.eEnemy)) $ do
                           case self^.eMoveTarget of
                             Just _ -> modifyRef selfRef (\v -> v & eGoalEntity .~ (self^.eMoveTarget))
                             Nothing -> modifyRef selfRef (\v -> v & eGoalEntity .~ Nothing)

                         modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiSoundTarget)))

                         when ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiTempStandGround /= 0) $
                           modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement (Constants.aiStandGround .|. Constants.aiTempStandGround))))

                         return Nothing

                       else do
                         modifyRef selfRef (\v -> v & eShowHostile .~ truncate levelTime + 1)
                         return (Just False)

                 | otherwise ->
                     return Nothing

        checkIfEnemyIsDead :: Quake Bool
        checkIfEnemyIsDead = do
          self <- readRef selfRef

          case self^.eEnemy of
            Nothing ->
              return True

            Just enemyRef -> do
              enemy <- readRef enemyRef

              if | not (enemy^.eInUse) ->
                     return True

                 | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiMedic /= 0 ->
                     if (enemy^.eHealth) > 0
                       then do
                         modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiMedic)))
                         return True
                       else
                         return False

                 | otherwise ->
                     return $ if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiBrutal /= 0
                                then if (enemy^.eHealth) <= -80
                                       then True
                                       else False
                                else if (enemy^.eHealth) <= 0
                                       then True
                                       else False

        lookForOtherTarget :: Quake Bool
        lookForOtherTarget = do
          modifyRef selfRef (\v -> v & eEnemy .~ Nothing)

          self <- readRef selfRef
          oldEnemy <- case self^.eOldEnemy of
                        Nothing -> return Nothing
                        Just oldEnemyRef -> liftM Just (readRef oldEnemyRef)

          if isJust oldEnemy && ((fromJust oldEnemy)^.eHealth) > 0
            then do
              modifyRef selfRef (\v -> v & eEnemy .~ (self^.eOldEnemy)
                                            & eOldEnemy .~ Nothing)

              huntTarget selfRef

              return False

            else do
              case self^.eMoveTarget of
                Just _ -> do
                  modifyRef selfRef (\v -> v & eGoalEntity .~ (self^.eMoveTarget))
                  void $ think (fromJust $ self^.eMonsterInfo.miWalk) selfRef

                Nothing -> do
                  -- we need the pausetime otherwise the stand code
                  -- will just revert to walking with no target and
                  -- the monsters will wonder around aimlessly trying
                  -- to hunt the world entity
                  levelTime <- use $ gameBaseGlobals.gbLevel.llTime
                  modifyRef selfRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + 100000000)
                  void $ think (fromJust $ self^.eMonsterInfo.miStand) selfRef

              return True

-- Decides running or standing according to flag AI_STAND_GROUND
huntTarget :: Ref EdictT -> Quake ()
huntTarget selfRef = do
    self <- readRef selfRef

    modifyRef selfRef (\v -> v & eGoalEntity .~ (self^.eEnemy))

    void $ if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
             then think (fromJust $ self^.eMonsterInfo.miStand) selfRef
             else think (fromJust $ self^.eMonsterInfo.miRun) selfRef

    self' <- readRef selfRef
    let Just enemyRef = self'^.eEnemy
    enemy <- readRef enemyRef

    let vec = (enemy^.eEntityState.esOrigin) - (self'^.eEntityState.esOrigin)

    modifyRef selfRef (\v -> v & eIdealYaw .~ Math3D.vectorYaw vec)

    -- wait a while before first attack
    when ((self'^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround == 0) $
      GameUtil.attackFinished selfRef 1

facingIdeal :: EdictT -> Bool
facingIdeal self =
    let delta = Math3D.angleMod ((self^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) - (self^.eIdealYaw))
    in if delta > 45 && delta < 315
         then False
         else True

-- Turn and close until within an angle to launch a melee attack.
aiRunMelee :: Ref EdictT -> Quake ()
aiRunMelee selfRef = do
    enemyYaw <- use $ gameBaseGlobals.gbEnemyYaw

    modifyRef selfRef (\v ->v & eIdealYaw .~ enemyYaw)
    M.changeYaw selfRef

    self <- readRef selfRef

    when (facingIdeal self) $ do
      void $ think (fromJust $ self^.eMonsterInfo.miAttack) selfRef
      modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ Constants.asStraight)

-- Turn in place until within an angle to launch a missile attack.
aiRunMissile :: Ref EdictT -> Quake ()
aiRunMissile selfRef = do
    enemyYaw <- use $ gameBaseGlobals.gbEnemyYaw

    modifyRef selfRef (\v -> v & eIdealYaw .~ enemyYaw)
    M.changeYaw selfRef

    self <- readRef selfRef

    when (facingIdeal self) $ do
      void $ think (fromJust $ self^.eMonsterInfo.miAttack) selfRef
      modifyRef selfRef (\v -> v & eMonsterInfo.miAttackState .~ Constants.asStraight)

-- Strafe sideways, but stay at aproximately the same range.
aiRunSlide :: Ref EdictT -> Float -> Quake ()
aiRunSlide selfRef distance = do
    enemyYaw <- use $ gameBaseGlobals.gbEnemyYaw

    modifyRef selfRef (\v -> v & eIdealYaw .~ enemyYaw)
    M.changeYaw selfRef

    self <- readRef selfRef

    let ofs = if (self^.eMonsterInfo.miLefty) /= 0
                then 90
                else -90

    ok <- M.walkMove selfRef ((self^.eIdealYaw) + ofs) distance

    unless ok $ do
      self' <- readRef selfRef
      modifyRef selfRef (\v -> v & eMonsterInfo.miLefty .~ 1 - (self'^.eMonsterInfo.miLefty))
      void $ M.walkMove selfRef ((self'^.eIdealYaw) - ofs) distance

flyMonsterStart :: EntThink
flyMonsterStart =
  GenericEntThink "flymonster_start" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eFlags %~ (\a -> a .|. Constants.flFly)
                                  & eThink .~ Just flyMonsterStartGo
                                  )
    Monster.monsterStart selfRef
    return True

flyMonsterStartGo :: EntThink
flyMonsterStartGo =
  GenericEntThink "flymonster_start_go" $ \selfRef -> do
    ok <- M.walkMove selfRef 0 0
    
    unless ok $ do
      self <- readRef selfRef
      dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
      dprintf ((self^.eClassName) `B.append` " in solid at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")
    
    modifyRef selfRef (\v -> v & eYawSpeed %~ (\a -> if a == 0 then 20 else a)
                                  & eViewHeight .~ 25
                                  )
    
    Monster.monsterStartGo selfRef
    
    self <- readRef selfRef
    
    when ((self^.eSpawnFlags) .&. 2 /= 0) $
      void $ think Monster.monsterTriggeredStart selfRef
    
    return True

swimMonsterStart :: EntThink
swimMonsterStart =
  GenericEntThink "swimmonster_start" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eFlags %~ (\a -> a .|. Constants.flSwim)
                                  & eThink .~ Just swimMonsterStartGo
                                  )
    Monster.monsterStart selfRef
    return True

swimMonsterStartGo :: EntThink
swimMonsterStartGo =
  GenericEntThink "swimmonster_start_go" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eYawSpeed %~ (\a -> if a == 0 then 20 else a)
                                  & eViewHeight .~ 10
                                  )
    
    Monster.monsterStartGo selfRef
    
    self <- readRef selfRef
    
    when ((self^.eSpawnFlags) .&. 2 /= 0) $
      void $ think Monster.monsterTriggeredStart selfRef
    
    return True
