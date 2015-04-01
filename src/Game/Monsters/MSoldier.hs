{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MSoldier where

import Quake
import Game.EntThink

spMonsterSoldier :: EntThink
spMonsterSoldier =
  GenericEntThink "SP_monster_soldier" $ \_ -> do
    io (putStrLn "MSoldier.spMonsterSoldier") >> undefined -- TODO

spMonsterSoldierSS :: EntThink
spMonsterSoldierSS =
  GenericEntThink "SP_monster_soldier_ss" $ \_ -> do
    io (putStrLn "MSoldier.spMonsterSoldierSS") >> undefined -- TODO

spMonsterSoldierLight :: EntThink
spMonsterSoldierLight =
  GenericEntThink "SP_monster_soldier_light" $ \_ -> do
    io (putStrLn "MSoldier.spMonsterSoldierLight") >> undefined -- TODO
