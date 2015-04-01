{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MTank where

import Quake
import Game.EntThink

spMonsterTank :: EntThink
spMonsterTank =
  GenericEntThink "SP_monster_tank" $ \_ -> do
    io (putStrLn "MTank.spMonsterTank") >> undefined -- TODO
