{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MBoss32 where

import Quake
import QuakeState
import Game.Adapters

frameStand201 :: Int
frameStand201 = 414

frameStand260 :: Int
frameStand260 = 473

makronToss :: EntThink
makronToss =
  GenericEntThink "MakronToss" $ \_ -> do
    io (putStrLn "MBoss32.makronToss") >> undefined -- TODO
