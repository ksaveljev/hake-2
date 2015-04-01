module Game.GameTurret where

import Quake
import QuakeState

spTurretBreach :: EdictReference -> Quake ()
spTurretBreach _ = io (putStrLn "GameTurret.spTurretBreach") >> undefined -- TODO

spTurretBase :: EdictReference -> Quake ()
spTurretBase _ = io (putStrLn "GameTurrent.spTurretBreach") >> undefined -- TODO

spTurretDriver :: EdictReference -> Quake ()
spTurretDriver _ = io (putStrLn "GameTurret.spTurretDriver") >> undefined -- TODO
