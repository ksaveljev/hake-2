module Game.Monsters.MFlyer where

import Quake
import QuakeState

spMonsterFlyer :: EdictReference -> Quake ()
spMonsterFlyer _ = io (putStrLn "MFlyer.spMonsterFlyer") >> undefined -- TODO
