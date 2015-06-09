{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MBoss32 where

import Quake
import QuakeState
import Game.Adapters

makronToss :: EntThink
makronToss =
  GenericEntThink "MakronToss" $ \_ -> do
    io (putStrLn "MBoss32.makronToss") >> undefined -- TODO
