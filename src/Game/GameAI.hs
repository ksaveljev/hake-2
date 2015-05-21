{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameAI where

import Control.Lens (use, (^.), ix, preuse, (.=))
import Control.Monad (void, when)
import Data.Bits ((.&.))
import Data.Maybe (isNothing, isJust)
import qualified Data.ByteString as B

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import qualified Game.Monster as Monster
import qualified Util.Lib as Lib

aiStand :: AI
aiStand =
  GenericAI "ai_stand" $ \_ _ -> do
    io (putStrLn "GameAI.aiStand") >> undefined -- TODO

aiCharge :: AI
aiCharge =
  GenericAI "ai_charge" $ \_ _ -> do
    io (putStrLn "GameAI.aiCharge") >> undefined -- TODO

aiMove :: AI
aiMove =
  GenericAI "ai_move" $ \_ _ -> do
    io (putStrLn "GameAI.aiMove") >> undefined -- TODO

aiWalk :: AI
aiWalk =
  GenericAI "ai_walk" $ \_ _ -> do
    io (putStrLn "GameAI.aiWalk") >> undefined -- TODO

aiRun :: AI
aiRun =
  GenericAI "ai_run" $ \_ _ -> do
    io (putStrLn "GameAI.aiRun") >> undefined -- TODO

walkMonsterStart :: EntThink
walkMonsterStart =
  GenericEntThink "walkmonster_start" $ \edictRef@(EdictReference edictIdx) -> do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaThink .= Just walkMonsterStartGo
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

          if | (edict^.eInUse) && (edict^.eEdictStatus.eHealth) > 0 && (edict^.eFlags) .&. Constants.flNoTarget == 0 ->
                 gameBaseGlobals.gbLevel.llSightClient .= Just (EdictReference check') -- got one
             | check' == start ->
                 gameBaseGlobals.gbLevel.llSightClient .= Nothing
             | otherwise -> lookThroughClients maxClients start check'

walkMonsterStartGo :: EntThink
walkMonsterStartGo =
  GenericEntThink "walkmonster_start_go" $ \selfRef@(EdictReference selfIdx) -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eSpawnFlags

    when (spawnFlags .&. 2 == 0 && levelTime < 1) $ do
      void $ think M.dropToFloor selfRef
      Just groundEntity <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoGroundEntity
      when (isJust groundEntity) $ do
        ok <- M.walkMove selfRef 0 0
        when (not ok) $ do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
          dprintf ((self^.eClassName) `B.append` " in solid at " `B.append` (Lib.vtos (self^.eEntityState.esOrigin)) `B.append` "\n")
          
    Just yawSpeed <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictPhysics.eYawSpeed
    when (yawSpeed == 0) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictPhysics.eYawSpeed .= 40

    gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictStatus.eViewHeight .= 25

    Monster.monsterStartGo selfRef

    Just spawnFlags' <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eSpawnFlags
    when (spawnFlags' .&. 2 /= 0) $
      void $ think Monster.monsterTriggeredStart selfRef

    return True
