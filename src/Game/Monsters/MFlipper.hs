module Game.Monsters.MFlipper where

import Quake
import QuakeState

spMonsterFlipper :: EdictReference -> Quake ()
spMonsterFlipper _ = io (putStrLn "MFlipper.spMonsterFlipper") >> undefined -- TODO
