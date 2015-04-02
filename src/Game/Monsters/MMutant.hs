{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MMutant where

import Quake
import Game.Adapters

spMonsterMutant :: EntThink
spMonsterMutant =
  GenericEntThink "SP_monster_mutant" $ \_ -> do
    io (putStrLn "MMutant.spMonsterMutant") >> undefined -- TODO
