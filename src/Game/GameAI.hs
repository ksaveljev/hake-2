{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameAI where

import Control.Lens (use, (^.), ix, preuse, (.=))
import Data.Bits ((.&.))
import Data.Maybe (isNothing)

import Quake
import QuakeState
import Game.Adapters
import qualified Constants

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
  GenericEntThink "walkmonster_start" $ \_ -> do
    io (putStrLn "GameAI.walkMonsterStart") >> undefined -- TODO

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
