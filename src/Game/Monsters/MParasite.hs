{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MParasite where

import Quake
import Game.Adapters

spMonsterParasite :: EntThink
spMonsterParasite =
  GenericEntThink "SP_monster_parasite" $ \_ -> do
    io (putStrLn "MParasite.spMonsterParasite") >> undefined -- TODO
