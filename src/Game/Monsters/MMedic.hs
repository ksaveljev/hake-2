module Game.Monsters.MMedic where

import Quake
import QuakeState

spMonsterMedic :: EdictReference -> Quake ()
spMonsterMedic _ = io (putStrLn "MMedic.spMonsterMedic") >> undefined -- TODO
