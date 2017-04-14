{-# LANGUAGE ScopedTypeVariables #-}
module Server.SVConsoleCommands
    ( initOperatorCommands
    ) where
 
import           Control.Exception     (IOException, handle)
import           Control.Lens          (use, (^.), (.=), (&), (.~))
import           Control.Monad         (when, void)
import           Data.Bits             ((.&.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Lazy     as HM
import qualified Data.Vector           as V
import           System.Directory      (copyFile, doesFileExist, removeFile)
import           System.FilePath       (takeFileName)
import           System.FilePath.Glob  (namesMatching)

import qualified Constants
import qualified Game.Cmd              as Cmd
import           Game.CVarT
import           Game.EdictT
import qualified Game.GameSave         as GameSave
import qualified Game.GameSVCmds       as GameSVCmds
import qualified Game.Info             as Info
import qualified QCommon.CM            as CM
import qualified QCommon.Com           as Com
import qualified QCommon.CVar          as CVar
import           QCommon.CVarVariables
import qualified QCommon.FSShared      as FS
import           QCommon.XCommandT     (runXCommandT)
import           QuakeRef
import           QuakeState
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVInit         as SVInit
import qualified Server.SVSend         as SVSend
import qualified Sys.NET               as NET
import           Types
import qualified Util.QuakeFile        as QuakeFile

import {-# SOURCE #-} qualified Server.SVMain as SVMain

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
initOperatorCommands = do
    Cmd.addInitialCommands initialCommands
    dedicatedValue <- fmap (^.cvValue) dedicatedCVar
    when (dedicatedValue /= 0) $
        Cmd.addCommand "say" (Just conSayF)

heartbeatF :: XCommandT
heartbeatF = XCommandT "SVConsoleCommands.heartbeatF" $
    svGlobals.svServerStatic.ssLastHeartbeat .= -9999999

kickF :: XCommandT
kickF = XCommandT "SVConsoleCommands.kickF" $ do
    initialized <- use (svGlobals.svServerStatic.ssInitialized)
    tryToKick initialized =<< Cmd.argc
  where
    tryToKick initialized c
        | not initialized = Com.printf "No server running.\n"
        | c /= 2 = Com.printf "Usage: kick <userid>\n"
        | otherwise = setPlayer >>= kick

kick :: Bool -> Quake ()
kick False = return ()
kick True = do
    clientRef <- use (svGlobals.svClient)
    maybe kickError proceedKicking clientRef
  where
    kickError = Com.fatalError "svGlobals.svClient is Nothing"

proceedKicking :: Ref ClientT -> Quake ()
proceedKicking clientRef = do
    client <- readRef clientRef
    SVSend.broadcastPrintf Constants.printHigh ((client^.cName) `B.append` " was kicked\n")
    -- print directly, because the dropped client won't get the SV_BroadcastPrintf message
    SVSend.clientPrintf client Constants.printHigh "You were kicked from the game\n"
    SVMain.dropClient clientRef
    realTime <- use (svGlobals.svServerStatic.ssRealTime)
    modifyRef clientRef (\v -> v & cLastMessage .~ realTime) -- min case there is a funny zombie

statusF :: XCommandT
statusF = XCommandT "SVConsoleCommands.statusF" $ do
    clients <- use (svGlobals.svServerStatic.ssClients)
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    proceedStatus clients maxClients
  where
    proceedStatus clients maxClients
        | V.null clients = Com.printf "No server running.\n"
        | otherwise = do
            serverName <- use (svGlobals.svServer.sName)
            Com.printf (B.concat ["map              : ", serverName, "\n"])
            Com.printf "num score ping name            lastmsg address               qport \n"
            Com.printf "--- ----- ---- --------------- ------- --------------------- ------\n"
            mapM_ printClientStatus (V.take maxClients clients)
            Com.printf "\n"

printClientStatus :: ClientT -> Quake ()
printClientStatus client
    | (client^.cState) == 0 = return ()
    | otherwise =
        error "SVConsoleCommands.printClientStatus" -- TODO

{-
		for (i = 0; i < SV_MAIN.maxclients.value; i++) {
			cl = SV_INIT.svs.clients[i];
			if (0 == cl.state)
				continue;

			Com.Printf("%3i ", new Vargs().add(i));
			Com.Printf("%5i ", new Vargs().add(cl.edict.client.ps.stats[Defines.STAT_FRAGS]));

			if (cl.state == Defines.cs_connected)
				Com.Printf("CNCT ");
			else if (cl.state == Defines.cs_zombie)
				Com.Printf("ZMBI ");
			else {
				ping = cl.ping < 9999 ? cl.ping : 9999;
				Com.Printf("%4i ", new Vargs().add(ping));
			}

			Com.Printf("%s", new Vargs().add(cl.name));
			l = 16 - cl.name.length();
			for (j = 0; j < l; j++)
				Com.Printf(" ");

			Com.Printf("%7i ", new Vargs().add(SV_INIT.svs.realtime - cl.lastmessage));

			s = NET.AdrToString(cl.netchan.remote_address);
			Com.Printf(s);
			l = 22 - s.length();
			for (j = 0; j < l; j++)
				Com.Printf(" ");

			Com.Printf("%5i", new Vargs().add(cl.netchan.qport));

			Com.Printf("\n");
		}
		-}

serverInfoF :: XCommandT
serverInfoF = XCommandT "SVConsoleCommands.serverInfoF" $ do
    Com.printf "Server info settings:\n"
    serverInfo <- CVar.serverInfo
    Info.printInfo serverInfo

dumpUserF :: XCommandT
dumpUserF = XCommandT "SVConsoleCommands.dumpUserF" $
    tryToDumpUser =<< Cmd.argc
  where
    tryToDumpUser c
        | c /= 2 = Com.printf "Usage: info <userid>\n"
        | otherwise = setPlayer >>= dumpUser

dumpUser :: Bool -> Quake ()
dumpUser False = return ()
dumpUser True = do
    Com.printf "userinfo\n"
    Com.printf "--------\n"
    clientRef <- use (svGlobals.svClient)
    maybe dumpUserError proceedDumpingUser clientRef
  where
    dumpUserError = Com.fatalError "svGlobals.svClient is Nothing"

proceedDumpingUser :: Ref ClientT -> Quake ()
proceedDumpingUser clientRef = do
    client <- readRef clientRef
    Info.printInfo (client^.cUserInfo)

mapF :: XCommandT
mapF = XCommandT "SVConsoleCommands.mapF" $ do
    mapName <- Cmd.argv 1
    fileLoaded <- tryLoadingMap mapName
    when fileLoaded startNewMap
  where
    tryLoadingMap mapName
        | BC.any (== '.') mapName = return True
        | otherwise = do
            let expandedMapName = expandMapName mapName
            len <- FS.fileLength expandedMapName -- IMPROVE: make sure this is enough. Jake2 uses FS.loadFile to verify
            maybe (loadMapError expandedMapName) (const (return True)) len
    expandMapName mapName = B.concat ["maps/", mapName, ".bsp"]
    loadMapError expandedMapName = do
        Com.printf (B.concat ["Can't find ", expandedMapName, "\n"])
        return False

startNewMap :: Quake ()
startNewMap = do
    svGlobals.svServer.sState .= Constants.ssDead
    wipeSaveGame "current"
    runXCommandT gameMapF

demoMapF :: XCommandT
demoMapF = XCommandT "SVConsoleCommands.demoMapF" $ do
    mapName <- Cmd.argv 1
    SVInit.svMap True mapName False

gameMapF :: XCommandT
gameMapF = XCommandT "SVConsoleCommands.gameMapF" $ do
    c <- Cmd.argc
    gameMap c
  where
    gameMap c
        | c /= 2 = Com.printf "USAGE: gamemap <map>\n"
        | otherwise = do
            -- check for clearing the current savegame
            mapName <- Cmd.argv 1
            Com.dprintf (B.concat ["SV_GameMap(", mapName, ")\n"])
            gd <- FS.gameDir
            FS.createPath (gd `B.append` "/save/current/")
            wipeOrSave mapName
            -- start up the next map
            SVInit.svMap False mapName False
            -- archive server state
            svGlobals.svServerStatic.ssMapCmd .= mapName
            -- copy off the level to the autosave slot
            dedicatedValue <- fmap (^.cvValue) dedicatedCVar
            when (dedicatedValue == 0) $ do
                writeServerFile True
                copySaveGame "current" "save0"
    wipeOrSave mapName
        | BC.head mapName == '*' = wipeSaveGame "current" -- wipe all the *.sav files
        | otherwise = do -- save the map just exited
            state <- use (svGlobals.svServer.sState)
            when (state == Constants.ssGame) $ do
                -- clear all the client inuse flags before saving so that
                -- when the level is re-entered, the clients will spawn
                -- at spawn points instead of occupying body shells
                maxClientsValue <- fmap (truncate . (^.cvValue)) maxClientsCVar
                clients <- use (svGlobals.svServerStatic.ssClients)
                savedInUse <- mapM (saveInUse clients) [0..maxClientsValue-1]
                writeLevelFile
                -- we must restore these for clients to transfer over correctly
                mapM_ (restoreInUse clients) ([0..] `zip` savedInUse)
    saveInUse clients idx = do
        maybe saveInUseError doSaveInUse ((clients V.! idx)^.cEdict)
    saveInUseError = do
        Com.fatalError "SVConsoleCommands.gameMapF#saveInUse edictRef is Nothing"
        return False
    doSaveInUse edictRef = do
        edict <- readRef edictRef
        modifyRef edictRef (\v -> v & eInUse .~ False)
        return (edict^.eInUse)
    restoreInUse clients (idx, inUse) =
        maybe restoreInUseError
              (\edictRef -> modifyRef edictRef (\v -> v & eInUse .~ inUse))
              ((clients V.! idx)^.cEdict)
    restoreInUseError =
        Com.fatalError "SVConsoleCommands.gameMapF#restorInuse edictRef is Nothing"

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
  where
    tryLoadingGame c
        | c /= 2 = Com.printf "Usage: loadgame <directory>\n"
        | otherwise = do
            Com.printf "Loading game...\n"
            gameDir <- FS.gameDir
            dir <- Cmd.argv 1
            checkBadSaveDir dir
            let name = B.concat [gameDir, "/save/", dir, "/server.ssv"]
            fileExists <- io (doesFileExist (BC.unpack name))
            loadGame dir name fileExists
    checkBadSaveDir dir =
        when (".." `B.isInfixOf` dir || "/" `B.isInfixOf` dir || "\\" `B.isInfixOf` dir) $
            Com.printf "Bad savedir.\n"

loadGame :: B.ByteString -> B.ByteString -> Bool -> Quake ()
loadGame _ name False = Com.printf (B.concat ["No such savegame: ", name, "\n"])
loadGame dir _ True = do
    copySaveGame dir "current"
    readServerFile
    svGlobals.svServer.sState .= Constants.ssDead
    mapCmd <- use (svGlobals.svServerStatic.ssMapCmd)
    SVInit.svMap False mapCmd True

killServerF :: XCommandT
killServerF = XCommandT "SVConsoleCommands.killServerF" $ do
    initialized <- use (svGlobals.svServerStatic.ssInitialized)
    when initialized shutdown
  where
    shutdown = do
        SVMain.shutdown "Server was killed.\n" False
        NET.config False -- close network sockets

serverCommandF :: XCommandT
serverCommandF = XCommandT "SVConsoleCommands.serverCommandF" GameSVCmds.serverCommand

conSayF :: XCommandT
conSayF = XCommandT "SVConsoleCommands.conSayF" $
    tryConSay =<< Cmd.argc
  where
    tryConSay c
        | c < 2 = return ()
        | otherwise = do
            arg <- Cmd.args
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

remove :: B.ByteString -> Quake ()
remove name = io $ handle (\(_ :: IOException) -> return ()) (removeFile (BC.unpack name))

wipeSaveGame :: B.ByteString -> Quake ()
wipeSaveGame saveName = do
    Com.dprintf (B.concat ["SV_WipeSaveGame(", saveName, ")\n"])
    gamedir <- FS.gameDir
    remove (B.concat [gamedir, "/save/", saveName, "/server.ssv"])
    remove (B.concat [gamedir, "/save/", saveName, "/game.ssv"])
    foundFiles <- io (namesMatching (BC.unpack (B.concat [gamedir, "/save/", saveName, "/*.sav"])))
    io (mapM_ removeFile foundFiles) -- IMPROVE: catch exceptions?
    otherFoundFiles <- io (namesMatching (BC.unpack (B.concat [gamedir, "/save/", saveName, "/*.sv2"])))
    io (mapM_ removeFile otherFoundFiles) -- IMPROVE: catch exceptions?

copySaveGame :: B.ByteString -> B.ByteString -> Quake ()
copySaveGame src dst = do
    Com.dprintf (B.concat ["SV_CopySaveGame(", src, ",", dst, ")\n"])
    wipeSaveGame dst
    -- copy the savegame over
    gamedir <- FS.gameDir
    let s1 = B.concat [gamedir, "/save/", src, "/server.ssv"]
        s2 = B.concat [gamedir, "/save/", dst, "/server.ssv"]
        name = B.concat [gamedir, "/save/", src, "/*.sav"]
    FS.createPath s2
    svCopyFile s1 s2
    svCopyFile (B.concat [gamedir, "/save/", src, "/game.ssv"])
               (B.concat [gamedir, "/save/", dst, "/game.ssv"])
    foundFiles <- io (namesMatching (BC.unpack name))
    mapM_ (copyFoundFile gamedir) foundFiles
  where
    copyFoundFile gamedir foundFile = do
        let foundFileB = BC.pack foundFile
            foundFileName = BC.pack (takeFileName foundFile)
            name = B.concat [gamedir, "/save/", dst, "/", foundFileName]
        svCopyFile foundFileB name
        svCopyFile (B.take (B.length foundFileB - 3) foundFileB `B.append` "sv2")
                   (B.take (B.length name - 3) name `B.append` "sv2")

readServerFile :: Quake ()
readServerFile = error "SVConsoleCommands.readServerFile" -- TODO

writeLevelFile :: Quake ()
writeLevelFile = do
    Com.dprintf "SV_WriteLevelFile()\n"
    gamedir <- FS.gameDir
    serverName <- use (svGlobals.svServer.sName)
    -- IMPROVE: catch exceptions?
    -- catch (Exception e) {
    --     Com.Printf("Failed to open " + name + "\n");
    --     e.printStackTrace();
    -- }
    --
    qf <- QuakeFile.open (B.concat [gamedir, "/save/current/", serverName, ".sv2"])
    maybe quakeFileError (doWriteLevelFile gamedir serverName) qf
  where
    quakeFileError = Com.fatalError "SVConsoleCommands.writeLevelFile qf is Nothing"
    doWriteLevelFile gamedir serverName qf = do
        configStrings <- fmap (V.take Constants.maxConfigStrings) (use (svGlobals.svServer.sConfigStrings))
        io (void (traverse (QuakeFile.writeString qf . Just) configStrings))
        CM.writePortalState qf
        QuakeFile.close qf
        GameSave.writeLevel (B.concat [gamedir, "/save/current/", serverName, ".sav"])

writeServerFile :: Bool -> Quake ()
writeServerFile autoSave = do
    Com.dprintf (B.concat ["SV_WriteServerFile(", if autoSave then "true" else "false", ")\n"])
    gameDir <- FS.gameDir
    -- IMPROVE: catch exception for the whole function block
    qf <- QuakeFile.open (gameDir `B.append` "/save/current/server.ssv")
    maybe serverFileError (doWriteServerFile gameDir) qf
  where
    serverFileError = Com.fatalError "SVConsoleCommands.writeServerFile failed to open QuakeFile"
    doWriteServerFile gameDir qf = do
        configStrings <- use (svGlobals.svServer.sConfigStrings)
        let comment
                | autoSave = "ENTERING " `B.append` (configStrings V.! Constants.csName)
                | otherwise = "TODO " `B.append` (configStrings V.! Constants.csName) -- TODO: add date/time stuff
        mapCmd <- use (svGlobals.svServerStatic.ssMapCmd)
        io $ do
            QuakeFile.writeString qf (Just comment)
            QuakeFile.writeString qf (Just mapCmd)
        vars <- fmap HM.elems (use (globals.gCVars))
        void (traverse (writeCVar qf) vars)
        io (QuakeFile.writeString qf Nothing)
        QuakeFile.close qf
        GameSave.writeGame (gameDir `B.append` "/save/current/game.ssv") autoSave
    writeCVar qf var
        | (var^.cvFlags) .&. Constants.cvarLatch == 0 =
            return ()
        | B.length (var^.cvName) >= Constants.maxOsPath || B.length (var^.cvString) >= 128 - 1 =
            Com.printf (B.concat ["Cvar too long: ", var^.cvName, " = ", var^.cvString, "\n"])
        | otherwise = io $ do
            -- IMPROVE: catch exception
            QuakeFile.writeString qf (Just (var^.cvName))
            QuakeFile.writeString qf (Just (var^.cvString))

svCopyFile :: B.ByteString -> B.ByteString -> Quake ()
svCopyFile src dst =
    io $ handle (\(_ :: IOException) -> return ()) (copyFile (BC.unpack src) (BC.unpack dst)) -- IMPROVE: Announce that copy file failed
