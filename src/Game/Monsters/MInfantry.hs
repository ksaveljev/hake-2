module Game.Monsters.MInfantry where

import Quake
import QuakeState

spMonsterInfantry :: EdictReference -> Quake ()
spMonsterInfantry _ = io (putStrLn "MInfantry.spMonsterInfantry") >> undefined -- TODO
