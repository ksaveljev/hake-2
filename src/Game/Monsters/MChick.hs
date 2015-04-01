module Game.Monsters.MChick where

import Quake
import QuakeState

spMonsterChick :: EdictReference -> Quake ()
spMonsterChick _ = io (putStrLn "MChick.spMonsterChick") >> undefined -- TODO
