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
    Cmd.addCommand "heartbeat" (Just heartbeatF)
    Cmd.addCommand "kick" (Just kickF)
    Cmd.addCommand "status" (Just statusF)
    Cmd.addCommand "serverinfo" (Just serverInfoF)
    Cmd.addCommand "dumpuser" (Just dumpUserF)
    Cmd.addCommand "map" (Just mapF)
    Cmd.addCommand "demomap" (Just demoMapF)
    Cmd.addCommand "gamemap" (Just gameMapF)
    Cmd.addCommand "setmaster" (Just setMasterF)

    dedicatedValue <- use $ cvarGlobals.dedicated.cvValue

    when (dedicatedValue /= 0) $
      Cmd.addCommand "say" (Just conSayF)

    Cmd.addCommand "serverrecord" (Just serverRecordF)
    Cmd.addCommand "serverstop" (Just serverStopF)
    Cmd.addCommand "save" (Just saveGameF)
    Cmd.addCommand "load" (Just loadGameF)
    Cmd.addCommand "killserver" (Just killServerF)
    Cmd.addCommand "sv" (Just serverCommandF)

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
