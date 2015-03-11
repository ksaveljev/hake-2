{-# LANGUAGE OverloadedStrings #-}
module Server.SVMain where

import Data.Bits ((.|.))
import Control.Lens ((.=), use)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified QCommon.SZ as SZ
import qualified Server.SVConsoleCommands as SVConsoleCommands

-- only called at quake2.exe startup, not for each game
init :: Quake ()
init = do
    SVConsoleCommands.initOperatorCommands

    Just rconPasswordCVar <- CVar.get "rcon_password" "" 0
    svGlobals.svRconPassword .= rconPasswordCVar

    void $ CVar.get "skill" "1" 0
    void $ CVar.get "deathmatch" "0" Constants.cvarLatch
    void $ CVar.get "coop" "0" Constants.cvarLatch
    void $ CVar.get "dmflags" (BC.pack $ show Constants.dfInstantItems) Constants.cvarServerInfo -- IMPROVE: convert Int to ByteString using binary package?
    void $ CVar.get "fraglimit" "0" Constants.cvarServerInfo
    void $ CVar.get "timelimit" "0" Constants.cvarServerInfo
    void $ CVar.get "cheats" "0" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ CVar.get "protocol" (BC.pack $ show Constants.protocolVersion) (Constants.cvarServerInfo .|. Constants.cvarNoSet) -- IMPROVE: convert Int to ByteString using binary package?

    Just maxClientsCVar <- CVar.get "maxclients" "1" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    svGlobals.svMaxClients .= maxClientsCVar

    Just hostnameCVar <- CVar.get "hostname" "noname" (Constants.cvarServerInfo .|. Constants.cvarArchive)
    svGlobals.svHostname .= hostnameCVar

    Just timeoutCVar <- CVar.get "timeout" "125" 0
    svGlobals.svTimeout .= timeoutCVar

    Just zombieTimeCVar <- CVar.get "zombietime" "2" 0
    svGlobals.svZombieTime .= zombieTimeCVar

    Just showClampCVar <- CVar.get "showclamp" "0" 0
    svGlobals.svShowClamp .= showClampCVar

    Just pausedCVar <- CVar.get "paused" "0" 0
    svGlobals.svPaused .= pausedCVar

    Just timeDemoCVar <- CVar.get "timedemo" "0" 0
    svGlobals.svTimeDemo .= timeDemoCVar

    Just enforceTimeCVar <- CVar.get "sv_enforcetime" "0" 0
    svGlobals.svEnforceTime .= enforceTimeCVar

    Just allowDownloadCVar <- CVar.get "allow_download" "1" Constants.cvarArchive
    svGlobals.svAllowDownload .= allowDownloadCVar

    Just allowDownloadPlayersCVar <- CVar.get "allow_download_players" "0" Constants.cvarArchive
    svGlobals.svAllowDownloadPlayers .= allowDownloadPlayersCVar

    Just allowDownloadModelsCVar <- CVar.get "allow_download_models" "1" Constants.cvarArchive
    svGlobals.svAllowDownloadModels .= allowDownloadModelsCVar

    Just allowDownloadSoundsCVar <- CVar.get "allow_download_sounds" "1" Constants.cvarArchive
    svGlobals.svAllowDownloadSounds .= allowDownloadSoundsCVar

    Just allowDownloadMapsCVar <- CVar.get "allow_download_maps" "1" Constants.cvarArchive
    svGlobals.svAllowDownloadMaps .= allowDownloadMapsCVar

    Just noReloadCVar <- CVar.get "sv_noreload" "0" 0
    svGlobals.svNoReload .= noReloadCVar

    Just airAccelerateCVar <- CVar.get "sv_airaccelerate" "0" Constants.cvarLatch
    svGlobals.svAirAccelerate .= airAccelerateCVar

    Just publicServerCVar <- CVar.get "public" "0" 0
    svGlobals.svPublicServer .= publicServerCVar

    Just reconnectLimitCVar <- CVar.get "sv_reconnect_limit" "3" Constants.cvarArchive
    svGlobals.svReconnectLimit .= reconnectLimitCVar

    bufData <- use $ globals.netMessageBuffer
    SZ.init (globals.netMessage) bufData (B.length bufData)
