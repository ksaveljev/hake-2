module Server.SVConsoleCommands
  ( initOperatorCommands
  ) where

import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import qualified Game.GameSVCmds as GameSVCmds
import qualified Game.Info as Info
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.FSShared as FS
import           QCommon.XCommandT (runXCommandT)
import           QuakeRef
import           QuakeState
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVInit as SVInit
import qualified Server.SVMainShared as SVMain
import qualified Server.SVSend as SVSend
import qualified Sys.NET as NET
import           Types

import           Control.Lens (use, (^.), (.=), (&), (.~))
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import           System.Directory (doesFileExist)

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
     tryToKick initialized =<< Cmd.argc
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

proceedKicking :: Ref ClientT -> Quake ()
proceedKicking clientRef =
  do client <- readRef clientRef
     SVSend.broadcastPrintf Constants.printHigh ((client^.cName) `B.append` " was kicked\n")
     -- print directly, because the dropped client won't get the SV_BroadcastPrintf message
     SVSend.clientPrintf client Constants.printHigh "You were kicked from the game\n"
     SVMain.dropClient clientRef
     realTime <- use (svGlobals.svServerStatic.ssRealTime)
     modifyRef clientRef (\v -> v & cLastMessage .~ realTime) -- min case there is a funny zombie

statusF :: XCommandT
statusF = XCommandT "SVConsoleCommands.statusF" $
  error "SVConsoleCommands.statusF" -- TODO

serverInfoF :: XCommandT
serverInfoF = XCommandT "SVConsoleCommands.serverInfoF" $
  do Com.printf "Server info settings:\n"
     serverInfo <- CVar.serverInfo
     Info.printInfo serverInfo

dumpUserF :: XCommandT
dumpUserF = XCommandT "SVConsoleCommands.dumpUserF" $
  tryToDumpUser =<< Cmd.argc
  where tryToDumpUser c
          | c /= 2 = Com.printf "Usage: info <userid>\n"
          | otherwise =
              do ok <- setPlayer
                 dumpUser ok

dumpUser :: Bool -> Quake ()
dumpUser False = return ()
dumpUser True =
  do Com.printf "userinfo\n"
     Com.printf "--------\n"
     clientRef <- use (svGlobals.svClient)
     maybe dumpUserError proceedDumpingUser clientRef
  where dumpUserError = Com.fatalError "svGlobals.svClient is Nothing"

proceedDumpingUser :: Ref ClientT -> Quake ()
proceedDumpingUser clientRef =
  do client <- readRef clientRef
     Info.printInfo (client^.cUserInfo)

mapF :: XCommandT
mapF = XCommandT "SVConsoleCommands.mapF" $
  do mapName <- Cmd.argv 1
     fileLoaded <- tryLoadingMap mapName
     when fileLoaded startNewMap
  where tryLoadingMap mapName
          | BC.any (== '.') mapName = return True
          | otherwise =
              do let expandedMapName = expandMapName mapName
                 len <- FS.fileLength expandedMapName -- IMPROVE: make sure this is enough. Jake2 uses FS.loadFile to verify
                 maybe (loadMapError expandedMapName) (const (return True)) len
        expandMapName mapName = B.concat ["maps/", mapName, ".bsp"]
        loadMapError expandedMapName =
          do Com.printf (B.concat ["Can't find ", expandedMapName, "\n"])
             return False

startNewMap :: Quake ()
startNewMap =
  do svGlobals.svServer.sState .= Constants.ssDead
     wipeSaveGame "current"
     runXCommandT gameMapF

demoMapF :: XCommandT
demoMapF = XCommandT "SVConsoleCommands.demoMapF" $
  do mapName <- Cmd.argv 1
     SVInit.svMap True mapName False

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
  tryLoadingGame =<< Cmd.argc
  where tryLoadingGame c
          | c /= 2 = Com.printf "Usage: loadgame <directory>\n"
          | otherwise =
              do Com.printf "Loading game...\n"
                 gameDir <- FS.gameDir
                 dir <- Cmd.argv 1
                 checkBadSaveDir dir
                 let name = B.concat [gameDir, "/save/", dir, "/server.ssv"]
                 fileExists <- request (io (doesFileExist (BC.unpack name)))
                 loadGame dir name fileExists
        checkBadSaveDir dir =
          when (".." `B.isInfixOf` dir || "/" `B.isInfixOf` dir || "\\" `B.isInfixOf` dir) $
            Com.printf "Bad savedir.\n"

loadGame :: B.ByteString -> B.ByteString -> Bool -> Quake ()
loadGame _ name False = Com.printf (B.concat ["No such savegame: ", name, "\n"])
loadGame dir _ True =
  do copySaveGame dir "current"
     readServerFile
     svGlobals.svServer.sState .= Constants.ssDead
     mapCmd <- use (svGlobals.svServerStatic.ssMapCmd)
     SVInit.svMap False mapCmd True

killServerF :: XCommandT
killServerF = XCommandT "SVConsoleCommands.killServerF" $
  do initialized <- use (svGlobals.svServerStatic.ssInitialized)
     when initialized shutdown
  where shutdown =
          do SVMain.shutdown "Server was killed.\n" False
             NET.config False -- close network sockets

serverCommandF :: XCommandT
serverCommandF = XCommandT "SVConsoleCommands.serverCommandF" GameSVCmds.serverCommand

conSayF :: XCommandT
conSayF = XCommandT "SVConsoleCommands.conSayF" $
  tryConSay =<< Cmd.argc
  where tryConSay c
          | c < 2 = return ()
          | otherwise =
              do arg <- Cmd.args
                 num <- fmap (truncate . (^.cvValue)) maxClientsCVar
                 clients <- fmap (V.take num) (use (svGlobals.svServerStatic.ssClients))
                 V.mapM_ (sendMessage (buildText arg)) clients
        buildText arg = B.concat ["console: ", stripText arg, "\n"]
        stripText arg
          | BC.head arg == '"' = B.take (B.length arg - 2) (B.drop 1 arg)
          | otherwise = arg

sendMessage :: B.ByteString -> ClientT -> Quake ()
sendMessage text client
  | client^.cState == Constants.csSpawned =
      SVSend.clientPrintf client Constants.printChat text
  | otherwise = return ()

setPlayer :: Quake Bool
setPlayer = error "SVConsoleCommands.setPlayer" -- TODO

wipeSaveGame :: B.ByteString -> Quake ()
wipeSaveGame = error "SVConsoleCommands.wipeSaveGame" -- TODO

copySaveGame :: B.ByteString -> B.ByteString -> Quake ()
copySaveGame = error "SVConsoleCommands.copySaveGame" -- TODO

readServerFile :: Quake ()
readServerFile = error "SVConsoleCommands.readServerFile" -- TODO