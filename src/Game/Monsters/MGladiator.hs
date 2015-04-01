module Game.Monsters.MGladiator where

import Quake
import QuakeState

spMonsterGladiator :: EdictReference -> Quake ()
spMonsterGladiator _ = io (putStrLn "MGladiator.spMonsterGladiator") >> undefined -- TODO
