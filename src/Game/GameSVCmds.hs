module Game.GameSVCmds where

import Quake

serverCommand :: Quake ()
serverCommand = io (putStrLn "GameSVCmds.serverCommand") >> undefined -- TODO
