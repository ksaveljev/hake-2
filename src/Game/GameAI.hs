{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameAI where

import Control.Lens (use, (^.), ix, preuse, (.=), (%=))
import Control.Monad (void, when, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.ByteString as B

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import qualified Game.GameUtil as GameUtil
import qualified Game.Monster as Monster
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

aiRun :: AI
aiRun =
  GenericAI "ai_run" $ \_ _ -> do
    io (putStrLn "GameAI.aiRun") >> undefined -- TODO

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
