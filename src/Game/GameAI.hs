{-# LANGUAGE OverloadedStrings #-}
module Game.GameAI where

import Quake
import Game.Adapters

walkMonsterStart :: EntThink
walkMonsterStart =
  GenericEntThink "walkmonster_start" $ \_ -> do
    io (putStrLn "GameAI.walkMonsterStart") >> undefined -- TODO
