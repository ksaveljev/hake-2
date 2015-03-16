{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Server.SVConsoleCommands where

import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Foldable (find)
import Data.Traversable (traverse)
import Control.Lens (use, (.=), (^.), (%=))
import Control.Lens.At (ix)
import Control.Monad (when, void, liftM)
import System.Directory (doesFileExist, removeFile, copyFile)
import System.FilePath (takeFileName)
import System.FilePath.Glob (namesMatching)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.NetAdrT
import QCommon.XCommandT
import qualified Constants
import qualified Server.SVInit as SVInit
import qualified Server.SVMain as SVMain
import qualified Server.SVSend as SVSend
import qualified Game.Cmd as Cmd
import qualified Game.GameSave as GameSave
import qualified Game.GameSVCmds as GameSVCmds
import qualified Game.Info as Info
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import qualified QCommon.NetChannel as NetChannel
import qualified Sys.NET as NET
import qualified Util.Lib as Lib
import qualified Util.QuakeFile as QuakeFile

{-
===============================================================================

OPERATOR CONSOLE ONLY COMMANDS

These commands can only be entered from stdin or by a remote operator datagram
===============================================================================
-}

{-
====================
SV_SetMaster_f

Specify a list of master servers
====================
-}
setMasterF :: XCommandT
setMasterF = do
    dedicatedValue <- use $ cvarGlobals.dedicated.cvValue

    -- only dedicated servers send heartbeats
    if dedicatedValue == 0
      then Com.printf "Only dedicated servers use masters.\n"
      else do
        -- make sure the server is listed public
        void $ CVar.set "public" "1"

        masters <- use $ svGlobals.svMasterAdr
        let idmaster = masters V.! 0
        svGlobals.svMasterAdr .= V.fromList (idmaster : map (const newNetAdrT) [1..Constants.maxMasters-1])

        c <- Cmd.argc
        newMasters <- collectMasters c 1 1 [] -- slot 0 will always contain the id master

        svGlobals.svMasterAdr %= (V.// newMasters)

        svGlobals.svServerStatic.ssLastHeartbeat .= -9999999

  where collectMasters :: Int -> Int -> Int -> [(Int, NetAdrT)] -> Quake [(Int, NetAdrT)]
        collectMasters argc idx slot accum
          | idx == argc = return accum
          | slot == Constants.maxMasters = return accum
          | otherwise = do
              vi <- Cmd.argv idx
              netAdr <- NET.stringToAdr vi
              case netAdr of
                Nothing -> do
                  Com.printf $ "Bad address: " `B.append` vi `B.append` "\n"
                  collectMasters argc (idx + 1) slot accum
                Just adr -> do
                  let masterAdr = if (adr^.naPort) == 0
                                    then adr { _naPort = Constants.portMaster }
                                    else adr

                  Com.printf $ "Master server at " `B.append` NET.adrToString masterAdr `B.append` "\n"
                  Com.printf "Sending a ping.\n"

                  NetChannel.outOfBandPrint Constants.nsServer masterAdr "ping"

                  collectMasters argc (idx + 1) (slot + 1) ((slot, masterAdr) : accum)

{-
==================
SV_SetPlayer

Sets sv_client and sv_player to the player with idnum Cmd.Argv(1)
==================
-}
setPlayer :: Quake Bool
setPlayer = do
    c <- Cmd.argc

    if c < 2
      then return False
      else do
        v1 <- Cmd.argv 1
        let ch = v1 `BC.index` 0

        maxClientsValue <- liftM truncate (use $ svGlobals.svMaxClients.cvValue)

        if isDigit ch
          then do
            let idnum = Lib.atoi v1
            if idnum < 0 || idnum >= maxClientsValue
              then do
                Com.printf $ "Bad client slot: " `B.append` BC.pack (show idnum) `B.append` "\n" -- IMPROVE: convert Int to ByteString using binary package?
                return False
              else do
                clients <- use $ svGlobals.svServerStatic.ssClients
                let client = clients V.! idnum
                svGlobals.svClient .= client
                svGlobals.svPlayer .= (client^.cEdict)

                if (client^.cState) == 0
                  then do
                    Com.printf $ "Client " `B.append` BC.pack (show idnum) `B.append` " is not active\n" -- IMPROVE: convert Int to ByteString using binary package?
                    return False
                  else return True
          else do -- check for a name match
            clients <- liftM (V.take maxClientsValue) (use $ svGlobals.svServerStatic.ssClients)
            let found = find (clientNameMatch v1) clients

            case found of
              Nothing -> do
                Com.printf $ "Userid " `B.append` v1 `B.append` " is not on the server\n"
                return False
              Just client -> do
                svGlobals.svClient .= client
                svGlobals.svPlayer .= (client^.cEdict)
                return True

  where clientNameMatch expectedName client =
          if | (client^.cState) == 0 -> False
             | (client^.cName) == expectedName -> True
             | otherwise -> False

{-
===============================================================================

SAVEGAME FILES

===============================================================================
-}

remove :: B.ByteString -> Quake ()
remove name = io $ removeFile (BC.unpack name) -- IMPROVE: catch exceptions ?

-- Delete save files save/(number)/.
wipeSaveGame :: B.ByteString -> Quake ()
wipeSaveGame savename = do
    Com.dprintf $ "SV_WipeSaveGame(" `B.append` savename `B.append` ")\n"

    gamedir <- FS.gameDir

    remove $ gamedir `B.append` "/save/" `B.append` savename `B.append` "/server.ssv"
    remove $ gamedir `B.append` "/save/" `B.append` savename `B.append` "/game.ssv"

    foundFiles <- io $ namesMatching $ BC.unpack $ gamedir `B.append` "/save/" `B.append` savename `B.append` "/*.sav"
    io $ mapM_ removeFile foundFiles -- IMPROVE: catch exceptions?

    otherFoundFiles <- io $ namesMatching $ BC.unpack $ gamedir `B.append` "/save/" `B.append` savename `B.append` "/*.sv2"
    io $ mapM_ removeFile otherFoundFiles -- IMPROVE: catch exceptions?

{-
================
CopyFile
================
-}
svCopyFile :: B.ByteString -> B.ByteString -> Quake ()
svCopyFile src dst = io $ copyFile (BC.unpack src) (BC.unpack dst) -- IMPROVE: catch exceptions ?

{-
================
SV_CopySaveGame
================
-}
copySaveGame :: B.ByteString -> B.ByteString -> Quake ()
copySaveGame src dst = do
    Com.dprintf $ "SV_CopySaveGame(" `B.append` src `B.append` "," `B.append` dst `B.append` ")\n"

    wipeSaveGame dst

    -- copy the savegame over
    gamedir <- FS.gameDir
    let s1 = gamedir `B.append` "/save/" `B.append` src `B.append` "/server.ssv"
        s2 = gamedir `B.append` "/save/" `B.append` dst `B.append` "/server.ssv"
    FS.createPath s2
    svCopyFile s1 s2

    let name_ = gamedir `B.append` "/save/" `B.append` src `B.append` "/game.ssv"
        name2_ = gamedir `B.append` "/save/" `B.append` dst `B.append` "/gamessv"
    svCopyFile name_ name2_

    let name = gamedir `B.append` "/save/" `B.append` src `B.append` "/*.sav"

    foundFiles <- io $ namesMatching (BC.unpack name)

    mapM_ (copyFoundFile gamedir) foundFiles

  where copyFoundFile gamedir foundFile = do
          let foundFileB = BC.pack foundFile
              foundFileName = BC.pack $ takeFileName foundFile
              name2 = gamedir `B.append` "/save/" `B.append` dst `B.append` "/" `B.append` foundFileName

          svCopyFile foundFileB name2

          let sv2name = B.take (B.length foundFileB - 3) foundFileB `B.append` "sv2"
              sv2name2 = B.take (B.length name2 - 3) name2 `B.append` "sv2"

          svCopyFile sv2name sv2name2

{-
==============
SV_WriteLevelFile

==============
-}
writeLevelFile :: Quake ()
writeLevelFile = do
    Com.dprintf "SV_WriteLevelFile()\n"

    gamedir <- FS.gameDir
    serverName <- use $ svGlobals.svServer.sName
    let sv2name = gamedir `B.append` "/save/current/" `B.append` serverName `B.append` ".sv2"

    -- IMPROVE: catch exceptions?
    -- catch (Exception e) {
    --     Com.Printf("Failed to open " + name + "\n");
    --     e.printStackTrace();
    -- }
    -- 
    qf <- io $ QuakeFile.open sv2name

    configStrings <- liftM (V.take Constants.maxConfigStrings) (use $ svGlobals.svServer.sConfigStrings)
    io $ void $ traverse (QuakeFile.writeString qf) configStrings

    CM.writePortalState qf

    io $ QuakeFile.close qf

    let name = gamedir `B.append` "/save/current/" `B.append` serverName `B.append` ".sav"
    GameSave.writeLevel name

{-
==============
SV_ReadLevelFile

==============
-}
readLevelFile :: Quake ()
readLevelFile = do
    Com.dprintf "SV_ReadLevelFile()\n"

    gamedir <- FS.gameDir
    serverName <- use $ svGlobals.svServer.sName
    let sv2name = gamedir `B.append` "/save/current/" `B.append` serverName `B.append` ".sv2"

    -- IMPROVE: catch exceptions?
    -- catch (Exception e) {
    --     Com.Printf("Failed to open " + name + "\n");
    --     e.printStackTrace();
    -- }
    -- 
    qf <- io $ QuakeFile.open sv2name

    configStrings <- io $ liftM V.fromList (mapM (const $ QuakeFile.readString qf) [0..Constants.maxConfigStrings-1])
    svGlobals.svServer.sConfigStrings .= configStrings

    CM.readPortalState qf

    io $ QuakeFile.close qf
    
    let name = gamedir `B.append` "/save/current/" `B.append` serverName `B.append` ".sav"
    GameSave.readLevel name

{-
==============
SV_WriteServerFile

==============
-}
writeServerFile :: Bool -> Quake ()
writeServerFile = undefined -- TODO

{-
==============
SV_ReadServerFile

==============
-}
readServerFile :: Quake ()
readServerFile = undefined -- TODO

{-
==================
SV_DemoMap_f

Puts the server in demo mode on a specific map/cinematic
==================
-}
demoMapF :: XCommandT
demoMapF = do
    v1 <- Cmd.argv 1
    SVInit.svMap True v1 False

{-
==================
SV_GameMap_f

Saves the state of the map just being exited and goes to a new map.

If the initial character of the map string is '*', the next map is
in a new unit, so the current savegame directory is cleared of
map files.

Example:

*inter.cin+jail

Clears the archived maps, plays the inter.cin cinematic, then
goes to map jail.bsp.
==================
-}
gameMapF :: XCommandT
gameMapF = do
    c <- Cmd.argc

    if c /= 2
      then Com.printf "USAGE: gamemap <map>\n"
      else do
        -- check for clearing the current savegame
        mapName <- Cmd.argv 1
        Com.dprintf $ "SV_GameMap(" `B.append` mapName `B.append` ")\n"

        gd <- FS.gameDir
        FS.createPath $ gd `B.append` "/save/current/"

        if BC.head mapName == '*'
          then wipeSaveGame "current" -- wipe all the *.sav files
          else do -- save the map just exited
            state <- use $ svGlobals.svServer.sState

            when (state == Constants.ssGame) $ do
              -- clear all the client inuse flags before saving so that
              -- when the level is re-entered, the clients will spawn
              -- at spawn points instead of occupying body shells
              maxClientsValue <- liftM truncate (use $ svGlobals.svMaxClients.cvValue) -- >>= return . truncate
              clients <- use $ svGlobals.svServerStatic.ssClients
              savedInUse <- mapM (\i -> do
                                        let inuse = (clients V.! i)^.cEdict.eInUse
                                        svGlobals.svServerStatic.ssClients.ix i.cEdict.eInUse .= False
                                        return inuse
                                ) [0..maxClientsValue-1]

              writeLevelFile

              -- we must restore these for clients to transfer over correctly
              mapM_ (\(i, inuse) ->
                      svGlobals.svServerStatic.ssClients.ix i.cEdict.eInUse .= inuse
                    ) ([0..] `zip` savedInUse)

        -- start up the next map
        SVInit.svMap False mapName False

        -- archive server state
        svGlobals.svServerStatic.ssMapCmd .= mapName

        -- copy off the level to the autosave slot
        dedicatedValue <- use $ cvarGlobals.dedicated.cvValue
        when (dedicatedValue == 0) $ do
          writeServerFile True
          copySaveGame "current" "save0"

{-
==================
SV_Map_f

Goes directly to a given map without any savegame archiving.
For development work
==================
-}
mapF :: XCommandT
mapF = do
    mapName <- Cmd.argv 1

    fileLoaded <-
      if BC.any (== '.') mapName
        then return $ Just () -- dirty hack :(
        else do
          let expanded = "maps/" `B.append` mapName `B.append` ".bsp"
          loadedFile <- FS.loadFile expanded
          case loadedFile of
            Just _ -> return $ Just ()
            Nothing -> do
              Com.printf $ "Can't find " `B.append` expanded `B.append` "\n"
              return Nothing

    when (isJust fileLoaded) $ do
      svGlobals.svServer.sState .= Constants.ssDead

      wipeSaveGame "current"
      gameMapF

{-
=====================================================================

  SAVEGAMES

=====================================================================
-}

{-
==============
SV_Loadgame_f

==============
-}
loadGameF :: XCommandT
loadGameF = do
    c <- Cmd.argc

    if c /= 2
      then Com.printf "USAGE: loadgame <directory>\n"
      else do
        Com.printf "Loading game...\n"
        
        dir <- Cmd.argv 1

        when (".." `B.isInfixOf` dir || "/" `B.isInfixOf` dir || "\\" `B.isInfixOf` dir) $
          Com.printf "Bad savedir.\n"

        gameDir <- FS.gameDir
        v1 <- Cmd.argv 1
        -- make sure the server.ssv file exists
        let name = gameDir `B.append` "/save/" `B.append` v1 `B.append` "/server.ssv"
        fileExists <- io $ doesFileExist (BC.unpack name)

        if fileExists
          then do
            copySaveGame v1 "current"
            readServerFile

            -- go to the map
            svGlobals.svServer.sState .= Constants.ssDead -- don't save current level when changing
            mapCmd <- use $ svGlobals.svServerStatic.ssMapCmd
            SVInit.svMap False mapCmd True
          else Com.printf $ "No such savegame: " `B.append` name `B.append` "\n"

{-
==============
SV_Savegame_f

==============
-}
saveGameF :: XCommandT
saveGameF = do
    state <- use $ svGlobals.svServer.sState
    c <- Cmd.argc
    deathmatchValue <- CVar.variableValue "deathmatch"
    v1 <- Cmd.argv 1
    maxClientsValue <- use $ svGlobals.svMaxClients.cvValue
    playerStats <- use $ svGlobals.svServerStatic.ssClients.ix 0.cEdict.eClient.gcPlayerState.psStats
    let health = playerStats UV.! Constants.statHealth

    if | state /= Constants.ssGame -> Com.printf "You must be in a game to save.\n"
       | c /= 2 -> Com.printf "USAGE: savegame <directory>\n"
       | deathmatchValue /= 0 -> Com.printf "Can't savegame in a deathmatch\n"
       | v1 == "current" -> Com.printf "Can't save to 'current'\n"
       | maxClientsValue == 1 && health <= 0 -> Com.printf "\nCan't savegame while dead!\n"
       | ".." `B.isInfixOf` v1 || "/" `B.isInfixOf` v1 || "\\" `B.isInfixOf` v1 -> Com.printf "Bad savedir.\n"
       | otherwise -> do
           Com.printf "Saving game...\n"

           -- archive current level, including all client edicts.
           -- when the level is reloaded, they will be shells awaiting
           -- a connecting client
           writeLevelFile

           -- // save server state
           -- IMPROVE: catch exception
           -- try {
           --     SV_WriteServerFile(false);
           -- }
           -- catch (Exception e) {
           --     Com.Printf("IOError in SV_WriteServerFile: " + e);
           -- }
           writeServerFile False

           -- copy it off
           copySaveGame "current" v1
           Com.printf "Done.\n"

-- ===============================================================
{-
==================
SV_Kick_f

Kick a user off of the server
==================
-}
kickF :: XCommandT
kickF = do
    initialized <- use $ svGlobals.svServerStatic.ssInitialized
    c <- Cmd.argc

    if | not initialized -> Com.printf "No server running.\n"
       | c /= 2 -> Com.printf "Usage: kick <userid>\n"
       | otherwise -> do
           sp <- setPlayer

           when sp $ do
             client <- use $ svGlobals.svClient
             let playerName = client^.cName
             SVSend.broadcastPrintf Constants.printHigh (playerName `B.append` " was kicked\n")
             -- print directly, because the dropped client won't get the
             -- SV_BroadcastPrintf message
             SVSend.clientPrintf client Constants.printHigh "You were kicked from the game\n"
             SVMain.dropClient client
             -- SV_INIT.svs.realtime
             realtime <- use $ svGlobals.svServerStatic.ssRealTime
             svGlobals.svClient.cLastMessage .= realtime -- min case there is a funny zombie

{-
================
SV_Status_f
================
-}
statusF :: XCommandT
statusF = undefined -- TODO

{-
==================
SV_ConSay_f
==================
-}
conSayF :: XCommandT
conSayF = undefined -- TODO

{-
==================
SV_Heartbeat_f
==================
-}
heartbeatF :: XCommandT
heartbeatF = svGlobals.svServerStatic.ssLastHeartbeat .= -9999999

{-
===========
SV_Serverinfo_f

  Examine or change the serverinfo string
===========
-}
serverInfoF :: XCommandT
serverInfoF = do
    Com.printf "Server info settings:\n"
    CVar.serverInfo >>= Info.print

{-
===========
SV_DumpUser_f

Examine all a users info strings
===========
-}
dumpUserF :: XCommandT
dumpUserF = do
    c <- Cmd.argc

    if c /= 2
      then Com.printf "Usage: info <userid>\n"
      else do
        sp <- setPlayer
        when sp $ do
          Com.printf "userinfo\n"
          Com.printf "--------\n"
          userInfo <- use $ svGlobals.svClient.cUserInfo
          Info.print userInfo

{-
==============
SV_ServerRecord_f

Begins server demo recording.  Every entity and every message will be
recorded, but no playerinfo will be stored.  Primarily for demo merging.
==============
-}
serverRecordF :: XCommandT
serverRecordF = undefined -- TODO

{-
==============
SV_ServerStop_f

Ends server demo recording
==============
-}
serverStopF :: XCommandT
serverStopF = undefined -- TODO

{-
===============
SV_KillServer_f

Kick everyone off, possibly in preparation for a new game

===============
-}
killServerF :: XCommandT
killServerF = do
    initialized <- use $ svGlobals.svServerStatic.ssInitialized
    when initialized $ do
      SVMain.shutdown "Server was killed.\n" False
      NET.config False -- close network sockets

{-
===============
SV_ServerCommand_f

Let the game dll handle a command
===============
-}
serverCommandF :: XCommandT
serverCommandF = GameSVCmds.serverCommand
-- ===========================================================

{-
==================
SV_InitOperatorCommands
==================
-}
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
