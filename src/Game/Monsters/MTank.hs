{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MTank where

import Quake
import Game.Adapters

spMonsterTank :: EntThink
spMonsterTank =
  GenericEntThink "SP_monster_tank" $ \_ -> do
    io (putStrLn "MTank.spMonsterTank") >> undefined -- TODO
