{-# LANGUAGE OverloadedStrings #-}
module Game.GameAI where

import Quake
import Game.Adapters

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

walkMonsterStart :: EntThink
walkMonsterStart =
  GenericEntThink "walkmonster_start" $ \_ -> do
    io (putStrLn "GameAI.walkMonsterStart") >> undefined -- TODO
