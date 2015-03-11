{-# LANGUAGE OverloadedStrings #-}
module Server.SVConsoleCommands where

import Control.Lens (use)
import Control.Monad (when)

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Game.Cmd as Cmd

initOperatorCommands :: Quake ()
initOperatorCommands = do
    Cmd.addCommand "heartbeat" heartbeatF
    Cmd.addCommand "kick" kickF
    Cmd.addCommand "status" statusF
    Cmd.addCommand "serverinfo" serverInfoF
    Cmd.addCommand "dumpuser" dumpUserF
    Cmd.addCommand "map" mapF
    Cmd.addCommand "demomap" demoMapF
    Cmd.addCommand "gamemap" gameMapF
    Cmd.addCommand "setmaster" setMasterF

    dedicatedValue <- use $ cvarGlobals.dedicated.cvValue

    when (dedicatedValue /= 0) $
      Cmd.addCommand "say" conSayF

    Cmd.addCommand "serverrecord" serverRecordF
    Cmd.addCommand "serverstop" serverStopF
    Cmd.addCommand "save" saveGameF
    Cmd.addCommand "load" loadGameF
    Cmd.addCommand "killserver" killServerF
    Cmd.addCommand "sv" serverCommandF

heartbeatF :: XCommandT
heartbeatF = undefined -- TODO

kickF :: XCommandT
kickF = undefined -- TODO

statusF :: XCommandT
statusF = undefined -- TODO

serverInfoF :: XCommandT
serverInfoF = undefined -- TODO

dumpUserF :: XCommandT
dumpUserF = undefined -- TODO

mapF :: XCommandT
mapF = undefined -- TODO

demoMapF :: XCommandT
demoMapF = undefined -- TODO

gameMapF :: XCommandT
gameMapF = undefined -- TODO

setMasterF :: XCommandT
setMasterF = undefined -- TODO

conSayF :: XCommandT
conSayF = undefined -- TODO

serverRecordF :: XCommandT
serverRecordF = undefined -- TODO

serverStopF :: XCommandT
serverStopF = undefined -- TODO

saveGameF :: XCommandT
saveGameF = undefined -- TODO

loadGameF :: XCommandT
loadGameF = undefined -- TODO

killServerF :: XCommandT
killServerF = undefined -- TODO

serverCommandF :: XCommandT
serverCommandF = undefined -- TODO
