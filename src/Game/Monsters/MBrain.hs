module Game.Monsters.MBrain where

import Quake
import QuakeState

spMonsterBrain :: EdictReference -> Quake ()
spMonsterBrain _ = io (putStrLn "MBrain.spMonsterBrain") >> undefined -- TODO
