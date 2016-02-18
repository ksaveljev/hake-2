module Server.SVConsoleCommands
  ( initOperatorCommands
  ) where

import qualified Game.Cmd as Cmd
import           QCommon.CVarVariables
import           QuakeState
import           Types

import           Control.Lens ((^.))
import           Control.Monad (when)
import qualified Data.ByteString as B

initOperatorCommands :: Quake ()
initOperatorCommands =
  do mapM_ (\(name, cmd) -> Cmd.addCommand name (Just cmd)) initialCommands
     dedicatedValue <- fmap (^.cvValue) dedicatedCVar
     when (dedicatedValue /= 0) $
       Cmd.addCommand "say" (Just conSayF)

initialCommands :: [(B.ByteString, XCommandT)]
initialCommands = [ ("heartbeat", heartbeatF)
                  , ("kick", kickF)
                  , ("status", statusF)
                  , ("serverinfo", serverInfoF)
                  , ("dumpuser", dumpUserF)
                  , ("map", mapF)
                  , ("demomap", demoMapF)
                  , ("gamemap", gameMapF)
                  , ("setmaster", setMasterF)
                  , ("serverrecord", serverRecordF)
                  , ("serverstop", serverStopF)
                  , ("save", saveGameF)
                  , ("load", loadGameF)
                  , ("killserver", killServerF)
                  , ("sv", serverCommandF)
                  ]


heartbeatF :: XCommandT
heartbeatF = XCommandT "SVConsoleCommands.heartbeatF" $
  error "SVConsoleCommands.heartbeatF" -- TODO

kickF :: XCommandT
kickF = XCommandT "SVConsoleCommands.kickF" $
  error "SVConsoleCommands.kickF" -- TODO

statusF :: XCommandT
statusF = XCommandT "SVConsoleCommands.statusF" $
  error "SVConsoleCommands.statusF" -- TODO

serverInfoF :: XCommandT
serverInfoF = XCommandT "SVConsoleCommands.serverInfoF" $
  error "SVConsoleCommands.serverInfoF" -- TODO

dumpUserF :: XCommandT
dumpUserF = XCommandT "SVConsoleCommands.dumpUserF" $
  error "SVConsoleCommands.dumpUserF" -- TODO

mapF :: XCommandT
mapF = XCommandT "SVConsoleCommands.mapF" $
  error "SVConsoleCommands.mapF" -- TODO

demoMapF :: XCommandT
demoMapF = XCommandT "SVConsoleCommands.demoMapF" $
  error "SVConsoleCommands.demoMapF" -- TODO

gameMapF :: XCommandT
gameMapF = XCommandT "SVConsoleCommands.gameMapF" $
  error "SVConsoleCommands.gameMapF" -- TODO

setMasterF :: XCommandT
setMasterF = XCommandT "SVConsoleCommands.setMasterF" $
  error "SVConsoleCommands.setMasterF" -- TODO

serverRecordF :: XCommandT
serverRecordF = XCommandT "SVConsoleCommands.serverRecordF" $
  error "SVConsoleCommands.serverRecordF" -- TODO

serverStopF :: XCommandT
serverStopF = XCommandT "SVConsoleCommands.serverStopF" $
  error "SVConsoleCommands.serverStopF" -- TODO

saveGameF :: XCommandT
saveGameF = XCommandT "SVConsoleCommands.saveGameF" $
  error "SVConsoleCommands.saveGameF" -- TODO

loadGameF :: XCommandT
loadGameF = XCommandT "SVConsoleCommands.loadGameF" $
  error "SVConsoleCommands.loadGameF" -- TODO

killServerF :: XCommandT
killServerF = XCommandT "SVConsoleCommands.killServerF" $
  error "SVConsoleCommands.killServerF" -- TODO

serverCommandF :: XCommandT
serverCommandF = XCommandT "SVConsoleCommands.serverCommandF" $
  error "SVConsoleCommands.serverCommandF" -- TODO

conSayF :: XCommandT
conSayF = XCommandT "SVConsoleCommands.conSayF" $
  error "SVConsoleCommands.conSayF" -- TODO