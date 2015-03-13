{-# LANGUAGE OverloadedStrings #-}
module Server.SVMain where

import Data.Bits ((.|.))
import Control.Lens (use)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified Server.SVConsoleCommands as SVConsoleCommands

-- only called at quake2.exe startup, not for each game
init :: Quake ()
init = do
    SVConsoleCommands.initOperatorCommands

    void $ CVar.getAndSet "rcon_password" "" 0 (svGlobals.svRconPassword)

    void $ CVar.get "skill" "1" 0
    void $ CVar.get "deathmatch" "0" Constants.cvarLatch
    void $ CVar.get "coop" "0" Constants.cvarLatch
    void $ CVar.get "dmflags" (BC.pack $ show Constants.dfInstantItems) Constants.cvarServerInfo -- IMPROVE: convert Int to ByteString using binary package?
    void $ CVar.get "fraglimit" "0" Constants.cvarServerInfo
    void $ CVar.get "timelimit" "0" Constants.cvarServerInfo
    void $ CVar.get "cheats" "0" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ CVar.get "protocol" (BC.pack $ show Constants.protocolVersion) (Constants.cvarServerInfo .|. Constants.cvarNoSet) -- IMPROVE: convert Int to ByteString using binary package?

    void $ CVar.getAndSet "maxclients" "1" (Constants.cvarServerInfo .|. Constants.cvarLatch) (svGlobals.svMaxClients)
    void $ CVar.getAndSet "hostname" "noname" (Constants.cvarServerInfo .|. Constants.cvarArchive) (svGlobals.svHostname)
    void $ CVar.getAndSet "timeout" "125" 0 (svGlobals.svTimeout)
    void $ CVar.getAndSet "zombietime" "2" 0 (svGlobals.svZombieTime)
    void $ CVar.getAndSet "showclamp" "0" 0 (svGlobals.svShowClamp)
    void $ CVar.getAndSet "paused" "0" 0 (svGlobals.svPaused)
    void $ CVar.getAndSet "timedemo" "0" 0 (svGlobals.svTimeDemo)
    void $ CVar.getAndSet "sv_enforcetime" "0" 0 (svGlobals.svEnforceTime)
    void $ CVar.getAndSet "allow_download" "1" Constants.cvarArchive (svGlobals.svAllowDownload)
    void $ CVar.getAndSet "allow_download_players" "0" Constants.cvarArchive (svGlobals.svAllowDownloadPlayers)
    void $ CVar.getAndSet "allow_download_models" "1" Constants.cvarArchive (svGlobals.svAllowDownloadModels)
    void $ CVar.getAndSet "allow_download_sounds" "1" Constants.cvarArchive (svGlobals.svAllowDownloadSounds)
    void $ CVar.getAndSet "allow_download_maps" "1" Constants.cvarArchive (svGlobals.svAllowDownloadMaps)
    void $ CVar.getAndSet "sv_noreload" "0" 0 (svGlobals.svNoReload)
    void $ CVar.getAndSet "sv_airaccelerate" "0" Constants.cvarLatch (svGlobals.svAirAccelerate)
    void $ CVar.getAndSet "public" "0" 0 (svGlobals.svPublicServer)
    void $ CVar.getAndSet "sv_reconnect_limit" "3" Constants.cvarArchive (svGlobals.svReconnectLimit)

    bufData <- use $ globals.netMessageBuffer
    SZ.init (globals.netMessage) bufData (B.length bufData)

-- Called when each game quits, before Sys_Quit or Sys_Error.
shutdown :: B.ByteString -> Bool -> Quake ()
shutdown = undefined -- TODO
