{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameAI where

import Control.Lens (use, (^.), ix, preuse, (.=), (%=), zoom)
import Control.Monad (void, when, unless, liftM)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (norm, _y)
import qualified Data.ByteString as B

import Quake
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
  GenericAI "ai_turn" $ \selfRef@(EdictReference selfIdx) dist -> do
    when (dist /= 0) $ do
      Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
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
  GenericAI "ai_stand" $ \selfRef@(EdictReference selfIdx) dist -> do
    when (dist /= 0) $ do
      Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
      void $ M.walkMove selfRef (self^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) dist

    checkAIStandGround selfRef
      >>= checkFindTarget selfRef
      >>= checkPauseTime selfRef
      >>= tryToIdle selfRef

  where checkAIStandGround :: EdictReference -> Quake Bool
        checkAIStandGround selfRef@(EdictReference selfIdx) = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

          if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
            then do
              case self^.eEnemy of
                Just (EdictReference enemyIdx) -> do
                  Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx
                  let v = (enemy^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
                      idealYaw = Math3D.vectorYaw v

                  gameBaseGlobals.gbGEdicts.ix selfIdx.eIdealYaw .= idealYaw

                  when ((self^.eEntityState.esAngles.(Math3D.v3Access Constants.yaw)) /= idealYaw && (self^.eMonsterInfo.miAIFlags) .&. Constants.aiTempStandGround /= 0) $ do
                    gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement (Constants.aiStandGround .|. Constants.aiTempStandGround)))
                    void $ think (fromJust $ self^.eMonsterInfo.miRun) selfRef

                  M.changeYaw selfRef
                  void $ aiCheckAttack selfRef 0

                Nothing ->
                  void $ GameUtil.findTarget selfRef
              
              return True
            else
              return False

        checkFindTarget :: EdictReference -> Bool -> Quake Bool
        checkFindTarget _ True = return True
        checkFindTarget selfRef _ = GameUtil.findTarget selfRef

        checkPauseTime :: EdictReference -> Bool -> Quake Bool
        checkPauseTime _ True = return True
        checkPauseTime selfRef@(EdictReference selfIdx) _ = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          if levelTime > (self^.eMonsterInfo.miPauseTime)
            then do
              void $ think (fromJust $ self^.eMonsterInfo.miWalk) selfRef
              return True
            else
              return False

        tryToIdle :: EdictReference -> Bool -> Quake ()
        tryToIdle _ True = return ()
        tryToIdle selfRef@(EdictReference selfIdx) _ = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          when ((self^.eSpawnFlags) .&. 1 == 0 && isJust (self^.eMonsterInfo.miIdle) && levelTime > (self^.eMonsterInfo.miIdleTime)) $ do
            rf <- Lib.randomF

            if (self^.eMonsterInfo.miIdleTime) /= 0
              then do
                void $ think (fromJust $ self^.eMonsterInfo.miIdle) selfRef
                gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miIdleTime .= levelTime + 15 + rf * 15
              else do
                gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miIdleTime .= levelTime + rf * 15

aiCharge :: AI
aiCharge =
  GenericAI "ai_charge" $ \_ _ -> do
    io (putStrLn "GameAI.aiCharge") >> undefined -- TODO

aiMove :: AI
aiMove =
  GenericAI "ai_move" $ \_ _ -> do
    io (putStrLn "GameAI.aiMove") >> undefined -- TODO

-- The monster is walking it's beat.
aiWalk :: AI
aiWalk =
  GenericAI "ai_walk" $ \selfRef@(EdictReference selfIdx) dist -> do
    -- io (print "GameAI.aiWalk!")
    -- io (print $ "dist = " ++ show dist)
    M.moveToGoal selfRef dist

    -- check for noticing a player
    found <- GameUtil.findTarget selfRef

    unless found $ do
      Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime

      when (isJust (self^.eMonsterInfo.miSearch) && levelTime > (self^.eMonsterInfo.miIdleTime)) $ do
        r <- Lib.randomF

        if (self^.eMonsterInfo.miIdleTime) /= 0
          then do
            void $ think (fromJust $ self^.eMonsterInfo.miSearch) selfRef
            gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miIdleTime .= levelTime + 15 + r * 15
          else
            gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miIdleTime .= levelTime + r * 15

-- The monster has an enemy it is trying to kill.
aiRun :: AI
aiRun =
  GenericAI "ai_run" $ \selfRef@(EdictReference selfIdx) dist -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    -- if we're going to a combat point, just proceed
    if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiCombatPoint /= 0
      then
        M.moveToGoal selfRef dist
      else do
        if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiSoundTarget /= 0
          then do
            let Just (EdictReference enemyIdx) = self^.eEnemy
            Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx
            let v = (self^.eEntityState.esOrigin) - (enemy^.eEntityState.esOrigin)
            -- ...and reached it
            if norm v < 64
              then do
                -- don't move, just stand and listen
                void $ think (fromJust $ self^.eMonsterInfo.miStand) selfRef
                -- since now it is aware and does not need to be triggered again.
                zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
                  eSpawnFlags %= (.&. (complement 1))
                  eEnemy .= Nothing
              else
                M.moveToGoal selfRef dist

            -- look for new targets
            ok <- GameUtil.findTarget selfRef
            when ok $
              checkAttack selfRef dist
          else
            checkAttack selfRef dist

  where checkAttack :: EdictReference -> Float -> Quake ()
        checkAttack selfRef dist = do
          attack <- aiCheckAttack selfRef dist
          unless attack $
            checkSliding selfRef dist

        checkSliding :: EdictReference -> Float -> Quake ()
        checkSliding selfRef@(EdictReference selfIdx) dist = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

          if (self^.eMonsterInfo.miAttackState) == Constants.asSliding
            then aiRunSlide selfRef dist
            else checkEnemyVis selfRef dist

        checkEnemyVis :: EdictReference -> Float -> Quake ()
        checkEnemyVis selfRef@(EdictReference selfIdx) dist = do
          enemyVis <- use $ gameBaseGlobals.gbEnemyVis

          if enemyVis
            then do
              M.moveToGoal selfRef dist

              Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
              let Just (EdictReference enemyIdx) = self^.eEnemy
              Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime

              zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
                eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiLostSight))
                eMonsterInfo.miLastSighting .= (enemy^.eEntityState.esOrigin)
                eMonsterInfo.miTrailTime .= levelTime
            else
              checkCoopEnemy selfRef dist

        checkCoopEnemy :: EdictReference -> Float -> Quake ()
        checkCoopEnemy selfRef@(EdictReference selfIdx) dist = do
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

        checkSearchTime :: EdictReference -> Float -> Quake ()
        checkSearchTime selfRef@(EdictReference selfIdx) dist = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          if (self^.eMonsterInfo.miSearchTime) /= 0 && levelTime > (self^.eMonsterInfo.miSearchTime + 20)
            then do
              M.moveToGoal selfRef dist
              gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miSearchTime .= 0
            else
              proceedRun selfRef dist

        proceedRun :: EdictReference -> Float -> Quake ()
        proceedRun selfRef@(EdictReference selfIdx) dist = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

          let save = self^.eGoalEntity
          tempGoal <- GameUtil.spawn

          gameBaseGlobals.gbGEdicts.ix selfIdx.eGoalEntity .= Just tempGoal

          new1 <- calcNew1 selfRef

          pursueNext selfRef dist new1

        calcNew1 :: EdictReference -> Quake Bool
        calcNew1 selfRef@(EdictReference selfIdx) = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

          new1 <- if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiLostSight == 0
                    then do
                      -- just lost sight of the player, decide where to go first
                      gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (\v -> (v .|. (Constants.aiLostSight .|. Constants.aiPursuitLastSeen)) .&. complement (Constants.aiPursueNext .|. Constants.aiPursueTemp))
                      return True
                    else
                      return False

          Just self' <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

          if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiPursueNext /= 0
            then do
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime

              zoom (gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo) $ do
                miAIFlags %= (.&. (complement Constants.aiPursueNext))
                -- give ourself more time since we got this far
                miSearchTime .= levelTime + 5

              (maybeMarker, new1') <- if | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiPursueTemp /= 0 -> do
                                             zoom (gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo) $ do
                                               miAIFlags %= (.&. (complement Constants.aiPursueTemp))
                                               miLastSighting .= (self^.eMonsterInfo.miSavedGoal)

                                             return (Nothing, True)

                                         | (self^.eMonsterInfo.miAIFlags) .&. Constants.aiPursuitLastSeen /= 0 -> do
                                             gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiPursuitLastSeen))
                                             marker <- PlayerTrail.pickFirst selfRef
                                             return (marker, new1)

                                         | otherwise -> do
                                             marker <- PlayerTrail.pickNext selfRef
                                             return (marker, new1)

              case maybeMarker of
                Nothing ->
                  return new1'

                Just (EdictReference markerIdx) -> do
                  Just marker <- preuse $ gameBaseGlobals.gbGEdicts.ix markerIdx
                  zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
                    eMonsterInfo.miTrailTime .= (marker^.eTimeStamp)
                    eEntityState.esAngles._y .= (marker^.eEntityState.esAngles._y) -- TODO: use Constants.yaw instead _y directly
                    eIdealYaw .= (marker^.eEntityState.esAngles._y) -- TODO: use Constants.yaw instead _y directly

                  return True
            else
              return new1

        pursueNext :: EdictReference -> Float -> Bool -> Quake ()
        pursueNext selfRef@(EdictReference selfIdx) dist new1 = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          let v = (self^.eEntityState.esOrigin) - (self^.eMonsterInfo.miLastSighting)
              d1 = norm v
          dist' <- if d1 <= dist
                     then do
                       gameBaseGlobals.gbGEdicts.ix selfIdx.eMonsterInfo.miAIFlags %= (.|. Constants.aiPursueNext)
                       return d1
                     else
                       return dist

          let Just (EdictReference goalEntityIdx) = self^.eGoalEntity
          gameBaseGlobals.gbGEdicts.ix goalEntityIdx.eEntityState.esOrigin .= (self^.eMonsterInfo.miLastSighting)

          correctCourse selfRef dist' new1

        correctCourse :: EdictReference -> Float -> Bool -> Quake ()
        correctCourse selfRef@(EdictReference selfIdx) dist new1 = do
          io (putStrLn "GameAI.aiRun#correctCourse") >> undefined -- TODO

walkMonsterStart :: EntThink
walkMonsterStart =
  GenericEntThink "walkmonster_start" $ \edictRef@(EdictReference edictIdx) -> do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eThink .= Just walkMonsterStartGo
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

    let start = if isNothing sightClient
                  then 1
                  else let Just (EdictReference idx) = sightClient
                       in idx
        check = start

    maxClientsValue <- use $ gameBaseGlobals.gbGame.glMaxClients

    lookThroughClients maxClientsValue start check

  where lookThroughClients :: Int -> Int -> Int -> Quake ()
        lookThroughClients maxClients start check = do
          let check' = if check + 1 > maxClients
                         then 1
                         else check + 1

          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix check'

          if | (edict^.eInUse) && (edict^.eHealth) > 0 && (edict^.eFlags) .&. Constants.flNoTarget == 0 ->
                 gameBaseGlobals.gbLevel.llSightClient .= Just (EdictReference check') -- got one
             | check' == start ->
                 gameBaseGlobals.gbLevel.llSightClient .= Nothing
             | otherwise -> lookThroughClients maxClients start check'

walkMonsterStartGo :: EntThink
walkMonsterStartGo =
  GenericEntThink "walkmonster_start_go" $ \selfRef@(EdictReference selfIdx) -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eSpawnFlags

    -- preuse (gameBaseGlobals.gbGEdicts.ix selfIdx) >>= \(Just blah) -> do
      -- io (print "GameAI: BEFORE")
      -- io (print $ "self.frame = " ++ show (blah^.eEntityState.esFrame))
      -- io (print $ "move.firstframe = " ++ show ((fromJust $ blah^.eMonsterInfo.miCurrentMove)^.mmFirstFrame))

    when (spawnFlags .&. 2 == 0 && levelTime < 1) $ do
      void $ think M.dropToFloor selfRef
      Just groundEntity <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eGroundEntity
      when (isJust groundEntity) $ do
        ok <- M.walkMove selfRef 0 0
        when (not ok) $ do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
          dprintf ((self^.eClassName) `B.append` " in solid at " `B.append` (Lib.vtos (self^.eEntityState.esOrigin)) `B.append` "\n")
          
    Just yawSpeed <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eYawSpeed
    when (yawSpeed == 0) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eYawSpeed .= 40

    gameBaseGlobals.gbGEdicts.ix selfIdx.eViewHeight .= 25

    Monster.monsterStartGo selfRef

    Just spawnFlags' <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eSpawnFlags
    when (spawnFlags' .&. 2 /= 0) $
      void $ think Monster.monsterTriggeredStart selfRef

    -- preuse (gameBaseGlobals.gbGEdicts.ix selfIdx) >>= \(Just blah) -> do
      -- io (print "GameAI: AFTER")
      -- io (print $ "self.frame = " ++ show (blah^.eEntityState.esFrame))
      -- io (print $ "move.firstframe = " ++ show ((fromJust $ blah^.eMonsterInfo.miCurrentMove)^.mmFirstFrame))

    return True

aiCheckAttack :: EdictReference -> Float -> Quake Bool
aiCheckAttack _ _ = do
    io (putStrLn "GameAI.aiCheckAttack") >> undefined -- TODO

-- Decides running or standing according to flag AI_STAND_GROUND
huntTarget :: EdictReference -> Quake ()
huntTarget selfRef@(EdictReference selfIdx) = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    gameBaseGlobals.gbGEdicts.ix selfIdx.eGoalEntity .= (self^.eEnemy)

    void $ if (self^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround /= 0
             then think (fromJust $ self^.eMonsterInfo.miStand) selfRef
             else think (fromJust $ self^.eMonsterInfo.miRun) selfRef

    Just self' <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just (EdictReference enemyIdx) = self'^.eEnemy
    Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx

    let vec = (enemy^.eEntityState.esOrigin) - (self'^.eEntityState.esOrigin)

    gameBaseGlobals.gbGEdicts.ix selfIdx.eIdealYaw .= Math3D.vectorYaw vec

    -- wait a while before first attack
    when ((self'^.eMonsterInfo.miAIFlags) .&. Constants.aiStandGround == 0) $
      GameUtil.attackFinished selfRef 1

aiRunSlide :: EdictReference -> Float -> Quake ()
aiRunSlide _ _ = do
    io (putStrLn "GameAI.aiRunSlide") >> undefined -- TODO
