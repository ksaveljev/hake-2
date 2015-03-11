module Server.SVMain where

import Quake
import qualified Server.SVConsoleCommands as SVConsoleCommands

-- only called at quake2.exe startup, not for each game
init :: Quake ()
init = do
    SVConsoleCommands.initOperatorCommands

    undefined -- TODO
