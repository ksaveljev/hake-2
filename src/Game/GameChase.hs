module Game.GameChase where

import Quake
import QuakeState

getChaseTarget :: EdictReference -> Quake ()
getChaseTarget _ = do
    io (putStrLn "GameChase.getChaseTarget") >> undefined -- TODO

updateChaseCam :: EdictReference -> Quake ()
updateChaseCam _ = do
    io (putStrLn "GameChase.updateChaseCam") >> undefined -- TODO
