{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MSuperTank where

import Quake
import Game.Adapters

spMonsterSuperTank :: EntThink
spMonsterSuperTank =
  GenericEntThink "SP_monster_supertank" $ \_ -> do
    io (putStrLn "MSuperTank.spMonsterSuperTank") >> undefined -- TODO