module Server.SVConsoleCommands
  ( initOperatorCommands
  ) where

import qualified Game.Cmd as Cmd
import qualified Game.Info as Info
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import qualified Server.SVInit as SVInit
import qualified Server.SVMainShared as SVMain
import qualified Sys.NET as NET
import           Types

import           Control.Lens (use, (^.), (.=), (&), (.~))
import           Control.Monad (when)
import qualified Data.ByteString as B

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("heartbeat", Just heartbeatF) , ("kick", Just kickF)
  , ("status", Just statusF), ("serverinfo", Just serverInfoF)
  , ("dumpuser", Just dumpUserF), ("map", Just mapF)
  , ("demomap", Just demoMapF), ("gamemap", Just gameMapF)
  , ("setmaster", Just setMasterF), ("serverrecord", Just serverRecordF)
  , ("serverstop", Just serverStopF), ("save", Just saveGameF)
  , ("load", Just loadGameF) , ("killserver", Just killServerF)
  , ("sv", Just serverCommandF)
  ]

initOperatorCommands :: Quake ()
initOperatorCommands =
  do Cmd.addInitialCommands initialCommands
     dedicatedValue <- fmap (^.cvValue) dedicatedCVar
     when (dedicatedValue /= 0) $
       Cmd.addCommand "say" (Just conSayF)

heartbeatF :: XCommandT
heartbeatF = XCommandT "SVConsoleCommands.heartbeatF" $
  svGlobals.svServerStatic.ssLastHeartbeat .= -9999999

kickF :: XCommandT
kickF = XCommandT "SVConsoleCommands.kickF" $
  do initialized <- use (svGlobals.svServerStatic.ssInitialized)
     c <- Cmd.argc
     tryToKick initialized c
  where tryToKick initialized c
          | not initialized = Com.printf "No server running.\n"
          | c /= 2 = Com.printf "Usage: kick <userid>\n"
          | otherwise =
              do ok <- setPlayer
                 kick ok

kick :: Bool -> Quake ()
kick False = return ()
kick True =
  do clientRef <- use (svGlobals.svClient)
     maybe kickError proceedKicking clientRef
  where kickError = Com.fatalError "svGlobals.svClient is Nothing"

proceedKicking :: ClientRef -> Quake ()
proceedKicking clientRef
  = do client <- readRef clientRef
       SVSend.broadcastPrintf Constants.printHigh ((client^.cName) `B.append` " was kicked\n")
       -- print directly, because the dropped client won't get the SV_BroadcastPrintf message
       SVSend.clientPring client Constants.printHigh "You were kicked from the game\n"
       SVMain.dropClient clientRef
       realTime <- use (svGlobals.svServerStatic.ssRealTime)
       modifyRef clientRef (\v -> v & cLastMessage .~ realTime) -- min case there is a funny zombie
{-
             Just clientRef@(ClientReference clientIdx) <- use $ svGlobals.svClient
             Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx
             let playerName = client^.cName
             SVSend.broadcastPrintf Constants.printHigh (playerName `B.append` " was kicked\n")
             -- print directly, because the dropped client won't get the
             -- SV_BroadcastPrintf message
             SVSend.clientPrintf client Constants.printHigh "You were kicked from the game\n"
             SVMain.dropClient clientRef
             -- SV_INIT.svs.realtime
             realtime <- use $ svGlobals.svServerStatic.ssRealTime
             svGlobals.svServerStatic.ssClients.ix clientIdx.cLastMessage .= realtime -- min case there is a funny zombie-}

statusF :: XCommandT
statusF = XCommandT "SVConsoleCommands.statusF" $
  error "SVConsoleCommands.statusF" -- TODO

serverInfoF :: XCommandT
serverInfoF = XCommandT "SVConsoleCommands.serverInfoF" $
  do Com.printf "Server info settings:\n"
     serverInfo <- CVar.serverInfo
     Info.printServerInfo serverInfo

dumpUserF :: XCommandT
dumpUserF = XCommandT "SVConsoleCommands.dumpUserF" $
  error "SVConsoleCommands.dumpUserF" -- TODO

mapF :: XCommandT
mapF = XCommandT "SVConsoleCommands.mapF" $
  error "SVConsoleCommands.mapF" -- TODO

demoMapF :: XCommandT
demoMapF = XCommandT "SVConsoleCommands.demoMapF" $
  do arg <- Cmd.argv 1
     SVInit.svMap True arg False

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
  do initialized <- use (svGlobals.svServerStatic.ssInitialized)
     when initialized shutdown
  where shutdown =
          do SVMain.shutdown "Server was killed.\n" False
             NET.config False -- close network sockets

serverCommandF :: XCommandT
serverCommandF = XCommandT "SVConsoleCommands.serverCommandF" $
  error "SVConsoleCommands.serverCommandF" -- TODO

conSayF :: XCommandT
conSayF = XCommandT "SVConsoleCommands.conSayF" $
  error "SVConsoleCommands.conSayF" -- TODO

setPlayer :: Quake Bool
setPlayer = error "SVConsoleCommands.setPlayer" -- TODO